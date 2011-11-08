// Lazy K interpreter in C
// For usage see usage() function below.
// Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
// Copyright 2011 Michael Sullivan.
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

#define DEBUG_COUNTERS 1

#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdbool.h>
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
typedef struct Expr Expr;

static void oom(int n);

// Garbage collection
#define MB (1024*1024)
#define HEAP_SIZE (64*MB)
static char space1[HEAP_SIZE];
static char space2[HEAP_SIZE];
static Expr *from_space_start = (Expr *)space1;
static Expr *from_space_end = (Expr *)(space1 + HEAP_SIZE);
static Expr *to_space_start = (Expr *)space2;
static Expr *to_space_end = (Expr *)(space2 + HEAP_SIZE);
static Expr *next_alloc = (Expr *)space1;//from_space_start;
static Expr **work_stack_top = (Expr **)(space1 + HEAP_SIZE);//(Expr **)from_space_end;


typedef enum Type { A, K, K1, S, S1, S2, I1, LazyRead, Inc, Num, Free } Type;


struct Expr {
	Expr *forward;
	union {
		Expr *arg1;
		int numeric_arg1; // XXX
	};
	Expr *arg2;
	Type type;
};

static inline Expr *alloc_expr() {
	INC_COUNTER(news);
	// We don't do an oom check. The caller better have already
	// done it with check or check_rooted.
	//if (next_alloc >= from_space_end) {
	//	oom(1);
	//}
	return next_alloc++;
}

static inline Expr *newExpr2(Type t, Expr *a1, Expr *a2) {
	struct Expr *e = alloc_expr();
	e->forward = 0;
	e->type = t;
	e->arg1 = a1; e->arg2 = a2;
	return e;
}
static inline Expr *newExpr1(Type t, Expr *a1) { return newExpr2(t, a1, NULL); }
static inline Expr *newExpr(Type t) { return newExpr2(t, NULL, NULL); }


static inline int to_number(Expr *e) {
	int result = (e->type == Num) ? e->numeric_arg1 : -1;
	return result;
}

#define Expr0(T) {NULL, NULL, NULL, T}
#define Expr1(T, ARG1) {NULL, ARG1, NULL, T}
#define Expr2(T, ARG1, ARG2) {NULL, ARG1, ARG2, T}


Expr cK = Expr0(K);
Expr cS = Expr0(S);
Expr cI = Expr2(S2, &cK, &cK);
Expr KI = Expr1(K1, &cI);

Expr SI = Expr1(S1, &cI);
Expr KS = Expr1(K1, &cS);
Expr KK = Expr1(K1, &cK);
Expr SKSK = Expr2(S2, &KS, &cK);

Expr cInc = Expr0(Inc);
Expr cZero = Expr0(Num);



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
static Expr *cached_church_chars[257];


static inline bool in_arena(Expr *p) {
	return p >= from_space_start && p < from_space_end;
}

static inline void push_work(Expr *e) {
	*(--work_stack_top) = e;
}
static inline Expr *pop_work() {
	return *work_stack_top++;
}

static inline Expr *copy_object(Expr *obj) {
	//assert(obj != (Expr*)(-2));
	if (!in_arena(obj)) return obj;
	if (obj->forward) {
		//fprintf(stderr, "%p -> %p\n", obj, obj->forward);
		return obj->forward;
	}

	*next_alloc = *obj;
	//obj->type = (Type)1337;
	//obj->arg1 = obj->arg2 = (Expr*)(-2);

	push_work(next_alloc);
	obj->forward = next_alloc;
	//fprintf(stderr, "forwarding %p to %p\n", obj, obj->forward);
	return next_alloc++;
}

static void gc() {
	INC_COUNTER(gcs);
	// Set up next_alloc to point into the to-space
	next_alloc = to_space_start;
	work_stack_top = (Expr **)to_space_end;

	// Process the roots
	for (int i = 0; i < root_stack_top; i++) {
		roots[i] = copy_object(roots[i]);
	}

	for (unsigned i = 0; i < sizeof(cached_church_chars)/sizeof(cached_church_chars[0]); i++) {
		cached_church_chars[i] = copy_object(cached_church_chars[i]);
	}

	while ((Expr *)work_stack_top != to_space_end) {
		//assert((Expr *)work_stack_top > next_alloc);
		Expr *cursor = pop_work();

		if (cursor->type != Num) {
			cursor->arg1 = copy_object(cursor->arg1);
			cursor->arg2 = copy_object(cursor->arg2);
		}
	}

	// Do the swap
	Expr *tmp = from_space_start;
	from_space_start = to_space_start;
	to_space_start = tmp;
	tmp = from_space_end;
	from_space_end = to_space_end;
	to_space_end = tmp;
}

static inline bool is_exhausted(int n) {
	return next_alloc + n >= from_space_end;
}

static void oom(int n) {
	gc();
	if (is_exhausted(n)) {
		fprintf(stderr, "out of memory!\n");
		abort();
	}
}

static inline void check(int n) {
	if (is_exhausted(n)) {
		oom(n);
	}
}

static inline void root(Expr *e) {
	roots[root_stack_top++] = e;
}
static inline Expr *unroot() {
	return roots[--root_stack_top];
}

static inline void check_rooted(int n, Expr **e1, Expr **e2) {
	if (is_exhausted(n)) {
		root(*e1);
		root(*e2);
		oom(n);
		*e2 = unroot();
		*e1 = unroot();
	}
}

static inline Expr *partial_apply(Expr *lhs, Expr *rhs) { // 1 alloc
	// You could do something more complicated here,
	// but I tried it and it didn't seem to improve
	// execution speed.
	return newExpr2(A, lhs, rhs);
}


Expr *make_church_char(int ch) {
	if (ch < 0 || ch > 256) {
		ch = 256;
	}

	if (cached_church_chars[ch] == 0) {
		if (ch == 0) {
			cached_church_chars[ch] = &KI;
		} else if (ch == 1) {
			cached_church_chars[ch] = &cI;
		} else {		
			cached_church_chars[ch] = newExpr2(S2, &SKSK, make_church_char(ch-1));
		}
	}
	return cached_church_chars[ch];
}

static inline Expr *drop_i1(Expr *cur) {
	// Seperating out this into two checks gets a real speed win.
	// Presumably due to branch prediction.
	if (cur->type == I1) {
		do {
			cur = cur->arg1;
		} while (cur->type == I1);
	}
	return cur;
}

static Expr *partial_eval(Expr *node);

// This function modifies the object in-place so that
// all references to it see the new version.
// An additional root gets past in by reference so that we can root it
// if we need to. I don't really like it but it is fast.
static inline Expr *partial_eval_primitive_application(Expr *e, Expr **prev) {
	INC_COUNTER(prim_apps);

	e->arg2 = drop_i1(e->arg2); // do it in place to free up space
	Expr *lhs = e->arg1, *rhs = e->arg2;

	switch (lhs->type) {
	case K: // 0 allocs
		e->type = K1;
		e->arg1 = rhs;
		e->arg2 = 0;
		break;
	case K1: // 0 allocs
		e->type = I1;
		e->arg1 = lhs->arg1;
		e->arg2 = 0;
		break;
	case S: // 0 allocs
		e->type = S1;
		e->arg1 = rhs;
		e->arg2 = 0;
		break;
	case S1: // 0 allocs
		e->type = S2;
		e->arg1 = lhs->arg1;
		e->arg2 = rhs;
		break;
	case LazyRead: // 6 allocs (4+2 from S2)
	{
		check_rooted(6, &e, prev);
		Expr *lhs = e->arg1;
		lhs->type = S2;
		lhs->arg1 = newExpr2(S2, &cI, newExpr1(K1, make_church_char(getchar())));
		lhs->arg2 = newExpr1(K1, newExpr(LazyRead));
		// fall thru
	}
	case S2: // 2 allocs
	{
		check_rooted(2, &e, prev);
		e->type = A; // XXX: Why was leaving this this out OK?
		Expr *lhs = e->arg1, *rhs = e->arg2;
		e->arg1 = partial_apply(lhs->arg1, rhs);
		e->arg2 = partial_apply(lhs->arg2, rhs);
		break;
	}
	case Inc: // 0 allocs - but recursion
	{
		// Inc is the one place we need to force evaluation of an rhs
		root(e);
		root(*prev);
		Expr *rhs_res = partial_eval(rhs);
		*prev = unroot();
		e = unroot();

		e->type = Num;
		e->numeric_arg1 = to_number(rhs_res) + 1;
		if (e->numeric_arg1 == 0) {
			fputs("Runtime error: invalid output format (attempted to apply inc to a non-number)\n",
			      stderr);
			abort();
		}
		e->arg2 = 0;
		break;
	}
	case Num:
		fputs("Runtime error: invalid output format (attempted to apply a number)\n", stderr);
		abort();
	default:
		fprintf(stderr,
		        "INTERNAL ERROR: invalid type in partial_eval_primitive_application (%d)\n",
		        e->arg1->type);
		abort();
	}

	return e;
}

/*
Expr *Expr::partial_eval() {
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

	Expr *prev = 0;
	Expr *cur = node;
	for (;;) {
		cur = drop_i1(cur);
		// Chase down the left hand side (while building a list of
		// where we came from linked through arg1) until we find
		// something that isn't an application. Once we have that,
		// we can apply the primitive, and then repeat.
		while (cur->type == A) {
			Expr *next = drop_i1(cur->arg1);
			cur->arg1 = prev;
			prev = cur; cur = next;
		}
		if (!prev) { // we've gotten it down to something that isn't an application
			break;
		}
		Expr *next = cur; cur = prev;
		prev = cur->arg1;
		cur->arg1 = next;

		cur = partial_eval_primitive_application(cur, &prev);
	}

	return cur;
}


Expr *parse_expr(FILE* f) {
	int ch;

	// Wait until we get something we care about
	do {
		ch = fgetc(f);
		if (ch == '#') {
			while ((ch = fgetc(f)) != '\n')
				;
		}
	} while (ch == '\n' || ch == ' ');
	
	switch (ch) {
	case '`':
	{
		Expr *p = parse_expr(f);
		Expr *q = parse_expr(f);
		return partial_apply(p, q);
	}
	case 'k': case 'K':
		return &cK;
	case 's': case 'S':
		return &cS;
	case 'i': case 'I':
		return &cI;
	default:
		printf("Invalid character!\n");
		abort();
	}
	return 0;
}

Expr *parse_expr_top(FILE* f) {
	Expr *e = parse_expr(f);
	if (fgetc(f) != '\n') {
		fprintf(stderr, "input program missing trailing newline\n");
		abort();
	}
	return e;
}

static Expr *car(Expr *list) {
	check(1);
	return partial_apply(list, &cK);
}

static Expr *cdr(Expr *list) {
	check(1);
	return partial_apply(list, &KI);
}

static int church2int(Expr *church) {
	check(2);
	Expr *e = partial_apply(partial_apply(church, &cInc), &cZero);
	*church2int_root = e;
	int result = to_number(partial_eval(e));
	if (result == -1) {
		fputs("Runtime error: invalid output format (result was not a number)\n", stderr);
		abort();
	}
	*church2int_root = 0;
	return result;
}


int main(int argc, char** argv) {
	// Preintialize the chuch numeral table
	for (unsigned i = 0; i < sizeof(cached_church_chars)/sizeof(cached_church_chars[0]); i++) {
		make_church_char(i);
	}

	FILE *f = stdin;
	if (argc == 2) {
		f = fopen(argv[1], "r");
		if (!f) {
			fprintf(stderr, "Unable to open the file \"%s\".\n", argv[1]);
			return 1;
		}
	}
	Expr *e = parse_expr_top(f);
	*toplevel_root = partial_apply(e, newExpr(LazyRead));

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
		fflush(stdout);
		*toplevel_root = cdr(*toplevel_root);
	}
}
