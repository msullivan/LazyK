// Lazy K interpreter in C++.
// For usage see usage() function below.
// Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
//
// Implementation notes:
//  - When Sxyz is reduced to (xz)(yz), both "copies" of z
//    point to the same expression tree. When z (or any of
//    its subexpressions) is reduced, the old tree nodes are
//    overwritten with their newly reduced versions, so that
//    any other pointers to the node get the benefit of the
//    change. This is critical to the performance of any
//    lazy evaluator. Despite this destructive update, the
//    meaning (i.e. behavior) of the function described by
//    any subtree never changes (until the nodes are
//    garbage-collected and reassigned, that is).
//  - I actually got stack overflows in the evaluator when
//    running complicated programs (e.g. prime_numbers.unl
//    inside the Unlambda interpreter), so I rewrote it to
//    eliminate recursion from partial_eval() and free().
//    These functions now use relatively abstruse iterative
//    algorithms which borrow expression tree pointers for
//    temporary storage, and restore the original values
//    where necessary before returning. Other than that, the
//    interpreter is pretty simple to understand. The only
//    recursion left (I think) is in the parser and in the
//    Inc case of partial_eval_primitive_application; the
//    former will only bite you if you have really deep
//    nesting in your source code, and the latter only if
//    you return a ridiculously large number in the output
//    stream.
//

#define DEBUG_COUNTERS 0

#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <ctype.h>

#if DEBUG_COUNTERS
static int news = 0;
static int gcs = 0;
static int prim_apps = 0;
static int part_apps = 0;

#define INC_COUNTER(n) ((n)++)
#else
#define INC_COUNTER(n)
#endif

struct Expr;

static void oom(int n);

// Garbage collection
#define MB (1024*1024)
#define HEAP_SIZE (64*MB)
static char space1[HEAP_SIZE];
static char space2[HEAP_SIZE];
static Expr *from_space_start = (Expr *)space1;
static Expr *from_space_end = (Expr *)(space1 + HEAP_SIZE);
static Expr *next_alloc = from_space_start;
static Expr *to_space_start = (Expr *)space2;

enum Type { A, K, K1, S, S1, S2, I1, LazyRead, Inc, Num, Free };

struct Expr {
	Expr *forward;
	union {
		Expr* arg1;
		int numeric_arg1;
	};
	Expr* arg2;
	Type type;

	static void* operator new(unsigned) {
		INC_COUNTER(news);
		if (next_alloc >= from_space_end) {
			oom(1);
		}
		return next_alloc++;
	}

	// caller keeps original ref
	Type gettype() { return type; }

	// caller loses refs to a1 and a2, gets ref to new object
	Expr(Type t, Expr* a1 =0, Expr* a2 =0) {
		forward = 0;
		type = t;
		arg1 = a1; arg2 = a2;
	}

	// caller loses original ref
	int to_number() {
		int result = (type == Num) ? numeric_arg1 : -1;
		return result;
	}
#if 0
	void print(Expr*);
#endif
	// caller loses original ref, gets returned ref
	Expr* drop_i1() {
		Expr* cur = this;
		if (type == I1) {
			do {
				cur = cur->arg1;
			} while (cur->type == I1);
		}
		return cur;
	}
};


Expr cK(K);
Expr cS(S);
Expr cI(S2, &cK, &cK);
Expr KI(K1, &cI);

Expr SI(S1, &cI);
Expr KS(K1, &cS);
Expr KK(K1, &cK);
Expr SKSK(S2, &KS, &cK);
Expr SIKS(S2, &cI, &KS);
Expr Iota(S2, &SIKS, &KK);

Expr cInc(Inc);
Expr cZero(Num);


// Roots
// we need 2 roots for toplevel and church2int,
// and then 2 per simultaneous invocation of partial_eval.
// partial_eval only recurses as deep as the biggest number printed,
// which can't /reasonably/ be above 512. This should be more than enough.
#define MAX_ROOTS 10000
static Expr *roots[MAX_ROOTS];
static Expr **toplevel_root = &roots[0];
static Expr **church2int_root = &roots[1];
static int root_stack_top = 2;
static Expr* cached_church_chars[257] = { &KI, &cI };


static inline bool in_arena(Expr *p) {
	return p >= from_space_start && p < from_space_end;
}

static inline Expr *copy_object(Expr *obj) {
	if (!in_arena(obj)) return obj;
	if (obj->forward) {
		//fprintf(stderr, "%p -> %p\n", obj, obj->forward);
		return obj->forward;
	}

	*next_alloc = *obj;
	obj->type = (Type)1337;
	obj->arg1 = obj->arg2 = (Expr*)(-1);

	obj->forward = next_alloc;
	//fprintf(stderr, "forwarding %p to %p\n", obj, obj->forward);
	return next_alloc++;
}

static void gc() {
	INC_COUNTER(gcs);
	// Set up next_alloc to point into the to-space
	next_alloc = to_space_start;

	// Process the roots
	for (int i = 0; i < root_stack_top; i++) {
		roots[i] = copy_object(roots[i]);
	}
	for (unsigned i = 0; i < sizeof(cached_church_chars)/sizeof(cached_church_chars[0]); i++) {
		cached_church_chars[i] = copy_object(cached_church_chars[i]);
	}

	Expr *cursor = to_space_start;
	while (cursor < next_alloc) {
		if (cursor->type != Num) {
			cursor->arg1 = copy_object(cursor->arg1);
			cursor->arg2 = copy_object(cursor->arg2);
		}
		cursor++;
	}

	// Do the swap
	Expr *tmp = from_space_start;
	from_space_start = to_space_start;
	to_space_start = tmp;
	from_space_end = (Expr *)((char *)from_space_start + HEAP_SIZE);
}

static inline bool is_exhausted(int n) {
	return next_alloc + n >= from_space_end;
}

static void oom(int n) {
	gc();
	if (is_exhausted(n)) {
		fprintf(stderr, "out of memory!\n");
		exit(4);
	}
}

static inline void check(int n) {
	if (is_exhausted(n)) {
		oom(n);
	}
}

static inline void check_rooted(int n, Expr *&e1, Expr *&e2) {
	if (is_exhausted(n)) {
		roots[root_stack_top++] = e1;
		roots[root_stack_top++] = e2;
		oom(n);
		root_stack_top -= 2;
		e1 = roots[root_stack_top];
		e2 = roots[root_stack_top+1];
	}
}

static inline Expr* partial_apply(Expr* lhs, Expr* rhs) { // 1 alloc
	// You could do something more complicated here,
	// but I tried it and it didn't seem to improve
	// execution speed.
	return new Expr(A, lhs, rhs);
}

#if 0
void Expr::print(Expr* highlight) {
	if (this == highlight) {
		fputs("###", stdout);
	}
	switch (type) {
		case A:
			putchar('(');
			arg1->print(highlight);
			putchar(' ');
			arg2->print(highlight);
			putchar(')');
			break;
		case K:
			putchar('K');
			break;
		case K1:
			fputs("[K ", stdout);
			arg1->print(highlight);
			putchar(']');
			break;
		case S:
			putchar('S');
			break;
		case S1:
			fputs("[s ", stdout);
			arg1->print(highlight);
			putchar(']');
			break;
		case S2:
			fputs("[S ", stdout);
			arg1->print(highlight);
			putchar(' ');
			arg2->print(highlight);
			putchar(']');
			break;
		case I1:
			putchar('.');
			arg1->print(highlight);
			break;
		case LazyRead:
			fputs("LazyRead", stdout);
			break;
		case Inc:
			fputs("Inc", stdout);
			break;
		case Num:
			printf("%d", numeric_arg1);
			break;
		default:
			putchar('?');
	}
	if (this == highlight) {
		fputs("###", stdout);
	}
}
#endif

Expr* make_church_char(int ch) {
	if (ch < 0 || ch > 256) {
		ch = 256;
	}

	if (cached_church_chars[ch] == 0) {
		cached_church_chars[ch] = new Expr(S2, &SKSK, make_church_char(ch-1));
	}
	return cached_church_chars[ch];
}


static Expr *partial_eval(Expr *node);

// This function modifies the object in-place so that
// all references to it see the new version.
void partial_eval_primitive_application() {
	INC_COUNTER(prim_apps);

	Expr *e = roots[root_stack_top-2];
	
	e->arg2 = e->arg2->drop_i1(); // do it in place to free up space
//	Expr *arg1 = e->arg1;
//	Expr *arg2 = e->arg2;
//	check(6);

	// arg1 and arg2 are now uninitialized space

	switch (e->arg1->type) {
	case K: // 0 allocs
		e->type = K1;
		e->arg1 = e->arg2;
		e->arg2 = 0;
		break;
	case K1: // 0 allocs
		e->type = I1;
		e->arg1 = e->arg1->arg1;
		e->arg2 = 0;
		break;
	case S: // 0 allocs
		e->type = S1;
		e->arg1 = e->arg2;
		e->arg2 = 0;
		break;
	case S1: // 0 allocs
		e->type = S2;
		e->arg1 = e->arg1->arg1;
		e->arg2 = e->arg2;
		break;
	case LazyRead: // 6 allocs (4+2 from S2)
		check(6); e = roots[root_stack_top-2];
		e->arg1->type = S2;
		e->arg1->arg1 = new Expr(S2, &cI, new Expr(K1, make_church_char(getchar())));
		e->arg1->arg2 = new Expr(K1, new Expr(LazyRead));
		// fall thru
	case S2: // 2 allocs
	{
		check(2); e = roots[root_stack_top-2];
		//type = A;
		Expr* lhs = e->arg1;
		Expr* rhs = e->arg2;
		e->arg1 = partial_apply(lhs->arg1, rhs);
		e->arg2 = partial_apply(lhs->arg2, rhs);
		break;
	}
	case Inc: // 0 allocs - but recursion
	{
		// Inc is the one place we need to force evaluation of an rhs
		Expr *arg2_new = partial_eval(e->arg2);
		e = roots[root_stack_top-2];
		e->arg2 = arg2_new;

		e->type = Num;
		e->numeric_arg1 = e->arg2->to_number() + 1;
		if (e->numeric_arg1 == 0) {
			fputs("Runtime error: invalid output format (attempted to apply inc to a non-number)\n", stderr);
			exit(3);
		}
		e->arg2 = 0;
		break;
	}
	case Num:
		fputs("Runtime error: invalid output format (attempted to apply a number)\n", stderr);
		exit(3);
	default:
		fprintf(stderr,
		        "INTERNAL ERROR: invalid type in partial_eval_primitive_application (%d)\n",
		        e->arg1->type);
		abort();
		exit(4);
	}
}

/*
Expr* Expr::partial_eval() {
	Expr *cur = this;
	for (;;) {
		cur = cur->drop_i1();
		if (cur->type != A) {
			return cur;
		}
		cur->arg1 = cur->arg1->partial_eval();
		cur->partial_eval_primitive_application();
	}
}
*/

// evaluates until the toplevel thing is not a function application.
// a stack of nodes that are waiting for their first argument to be evaluated is built,
// chained through the first argument field
static Expr *partial_eval(Expr *node) {
	INC_COUNTER(part_apps);

	Expr **cur_root = &roots[root_stack_top];
	Expr **prev_root = &roots[root_stack_top+1];
	root_stack_top += 2;
	
	Expr *prev = 0;
	Expr *cur = node;
	for (;;) {
		cur = cur->drop_i1();
		// Chase down the left hand side (while building a list of
		// where we came from linked through arg1) until we find
		// something that isn't an application. Once we have that,
		// we can apply the primitive, and then repeat.
		while (cur->type == A) {
			Expr* next = cur->arg1->drop_i1();
			cur->arg1 = prev;
			prev = cur; cur = next;
		}
		if (!prev) { // we've gotten it down to something that isn't an application
			break;
		}
		Expr* next = cur; cur = prev;
		prev = cur->arg1;
		cur->arg1 = next;

		*cur_root = cur;
		*prev_root = prev;
		partial_eval_primitive_application();
		cur = *cur_root;
		prev = *prev_root;
	}

	root_stack_top -= 2;
	return cur;
}

class Stream {
public:
	virtual int getch() = 0;
	virtual void ungetch(int ch) = 0;
	virtual void error(const char* msg) = 0;
};

class File : public Stream {
	FILE* f;
	const char* filename;
	enum { circular_buf_size = 256 };
	char circular_buf[circular_buf_size];
	int last_newline, cur_pos;
public:
	File(FILE* _f, const char* _filename) {
		f = _f; filename = _filename;
		last_newline = cur_pos = 0;
	}
	int getch();
	void ungetch(int ch);
	void error(const char* msg);
};

int File::getch() {
	int ch;
	do {
		ch = getc(f);
		circular_buf[(cur_pos++)%circular_buf_size] = ch;
		if (ch == '#') {
			do {
				ch = getc(f);
			} while (ch != '\n' && ch != EOF);
		}
		if (ch == '\n') {
			last_newline = cur_pos;
		}
	} while (isspace(ch));
	return ch;
}

void File::ungetch(int ch) {
	ungetc(ch, f);
	--cur_pos;
}

void File::error(const char* msg) {
	fprintf(stderr, "While parsing \"%s\": %s\n", filename, msg);
	int from;
	if (cur_pos-last_newline < circular_buf_size) {
		from = last_newline;
	} else {
		from = cur_pos-circular_buf_size+1;
		fputs("...", stdout);
	}
	for (int i=from; i < cur_pos; ++i) {
		putc(circular_buf[i%circular_buf_size], stderr);
	}
	fputs(" <--\n", stderr);
	exit(1);
}

class StringStream : public Stream {
	const char* str;
	const char* p;
public:
	StringStream(const char* s) {
		str = s; p = s;
	}
	int getch() {
		return *p ? *p++ : EOF;
	}
	void ungetch(int ch) {
		if (ch != EOF) --p;
	}
	void error(const char* msg) {
		fprintf(stderr, "While parsing command line: %s\n%s\n", msg, str);
		for (const char* q = str+1; q < p; ++q) {
			putc(' ', stderr);
		}
		fputs("^\n", stderr);
		exit(1);
	}
};


Expr* parse_expr(Stream* f, int ch, bool i_is_iota);

Expr* parse_manual_close(Stream* f, int expected_terminator);


Expr* parse_expr(Stream* f, int ch, bool i_is_iota) {
	switch (ch) {
	case '`': case '*':
	{
		Expr* p = parse_expr(f, f->getch(), ch=='*');
		Expr* q = parse_expr(f, f->getch(), ch=='*');
		return partial_apply(p, q);
	}
	case '(':
		return parse_manual_close(f, ')');
	case ')':
		f->error("Mismatched close-parenthesis!");
	case 'k': case 'K':
		return &cK;
	case 's': case 'S':
		return &cS;
	case 'i':
		if (i_is_iota)
			return &Iota;
		// else fall thru
	case 'I':
		return &cI;
	case '0': case '1':
	{
		Expr* e = &cI;
		do {
			if (ch == '0') {
				e = partial_apply(partial_apply(e, &cS), &cK);
			} else {
				e = partial_apply(&cS, partial_apply(&cK, e));
			}
			ch = f->getch();
		} while (ch == '0' || ch == '1');
		f->ungetch(ch);
		return e;
	}
	default:
		f->error("Invalid character!");
	}
	return 0;
}


Expr* parse_manual_close(Stream* f, int expected_terminator) {
	Expr* e = 0;
	int peek;
	while (peek = f->getch(), peek != ')' && peek != EOF) {
		Expr* e2 = parse_expr(f, peek, false);
		e = e ? partial_apply(e, e2) : e2;
	}
	if (peek != expected_terminator) {
		f->error(peek == EOF ? "Premature end of program!" : "Unmatched trailing close-parenthesis!");
	}
	if (e == 0) {
		e = &cI;
	}
	return e;
}


static Expr* car(Expr* list) {
	return partial_apply(list, &cK);
}

static Expr* cdr(Expr* list) {
	return partial_apply(list, &KI);
}

static int church2int(Expr* church) {
	check(2);
	Expr* e = partial_apply(partial_apply(church, &cInc), &cZero);
	*church2int_root = e;
	int result = partial_eval(e)->to_number();
	if (result == -1) {
		fputs("Runtime error: invalid output format (result was not a number)\n", stderr);
		exit(3);
	}
	*church2int_root = 0;
	return result;
}


Expr* compose(Expr* f, Expr* g) {
	return new Expr(S2, new Expr(K1, f), g);
}


Expr* append_program(Expr* old, Stream* stream) {
	return compose(parse_manual_close(stream, EOF), old);
}


void usage() {
	fputs(
		"usage: lazy [-b] { -e program | program-file.lazy } *\n"
		"\n"
		"   -b           puts stdin and stdout into binary mode on systems that care\n"
		"                (i.e. Windows)\n"
		"\n"
		"   -e program   takes program code from the command line (like Perl's -e\n"
		"                switch)\n"
		"\n"
		"   program-file.lazy   name of file containing program code\n"
		"\n"
		" If more than one -e or filename argument is given, the programs will be\n"
		" combined by functional composition (but in Unix pipe order, not mathematical-\n"
		" notation order). If no -e or filename argument is given, the result is a\n"
		" degenerate composition, i.e. the identity function.\n", stdout);
	exit(0);
}


int main(int argc, char** argv) {
	// Preintialize the chuch numeral table
	for (unsigned i = 0; i < sizeof(cached_church_chars)/sizeof(cached_church_chars[0]); i++) {
		make_church_char(i);
	}
	
	Expr* e = &cI;
	for (int i=1; i<argc; ++i) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 0:
			{
				File s(stdin, "(standard input)");
				e = append_program(e, &s);
				break;
			}
			case 'b':
				//setmode(fileno(stdin), O_BINARY);
				//setmode(fileno(stdout), O_BINARY);
				break;
			case 'e':
				++i;
				if (i == argc) {
					usage();
				}
				{
					StringStream s(argv[i]);
					e = append_program(e, &s);
					break;
				}
			default:
				usage();
			}
		} else {
			FILE* f = fopen(argv[i], "r");
			if (!f) {
				fprintf(stderr, "Unable to open the file \"%s\".\n", argv[i]);
				exit(1);
			}
			File s(f, argv[i]);
			e = append_program(e, &s);
		}
	}
	*toplevel_root = partial_apply(e, new Expr(LazyRead));
	for (;;) {
		int ch = church2int(car(*toplevel_root));
		if (ch >= 256) {
#if DEBUG_COUNTERS
			fprintf(stderr, "     gcs: %d\n    news: %d\n", gcs, news);
			fprintf(stderr, "primapps: %d\npartapps: %d\n", prim_apps, part_apps);
#endif
			return ch-256;
		}
		putchar(ch);
		*toplevel_root = cdr(*toplevel_root);
	}
}
