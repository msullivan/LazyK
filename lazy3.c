// Lazy K interpreter in C++.
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

#define DEBUG_COUNTERS 0

#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>

#define MB (1024*1024)
#define HEAP_SIZE (64*MB)
#if DEBUG_COUNTERS
#define INC_COUNTER(n) ((s->n)++)
#else
#define INC_COUNTER(n)
#endif
#define MAX_ROOTS 10000


struct Expr;
typedef struct Expr Expr;
typedef struct Expr * ExprP;

static void oom(int n);

typedef enum Type { A, K, K1, S, S1, S2, I1, LazyRead, Inc, Num, Free } Type;

struct Expr {
	ExprP forward;
	ExprP arg1;
	int numeric_arg1; // XXX
	ExprP arg2;
	Type type;
};

struct state_t {
	// Perf counters
	int news;
	int gcs;
	int prim_apps;
	int part_apps;

	// Garbage collection
	char space1[HEAP_SIZE];
	char space2[HEAP_SIZE];
	ExprP from_space_start;
	ExprP from_space_end;
	ExprP to_space_start;
	ExprP to_space_end;
	ExprP next_alloc;
	ExprP *work_stack_top;

	// Roots
	// we need 2 roots for toplevel and church2int,
	// and then 2 per simultaneous invocation of partial_eval.
	// partial_eval only recurses as deep as the biggest number printed,
	// which can't /reasonably/ be above 512. This should be more than enough.
	ExprP roots[MAX_ROOTS];
	ExprP *toplevel_root;
	ExprP *church2int_root;
	int root_stack_top;
	ExprP cached_church_chars[257];

	// Preconstructed terms
	Expr constants[10];
	
	ExprP cK;
	ExprP cS;
	ExprP cI;
	ExprP KI;

	ExprP SI;
	ExprP KS;
	ExprP KK;
	ExprP SKSK;

	ExprP cInc;
	ExprP cZero;
} state;
struct state_t *s = &state;


static inline ExprP alloc_expr() {
	INC_COUNTER(news);
	// We don't do an oom check. The caller better have already
	// done it with check or check_rooted.
	//if (next_alloc >= from_space_end) {
	//	oom(1);
	//}
	return s->next_alloc++;
}

static inline ExprP newExpr2(Type t, ExprP a1, ExprP a2) {
	ExprP e = alloc_expr();
	e->forward = 0;
	e->type = t;
	e->arg1 = a1; e->arg2 = a2;
	e->numeric_arg1 = 0;
	return e;
}
static inline ExprP newExpr1(Type t, ExprP a1) { return newExpr2(t, a1, NULL); }
static inline ExprP newExpr(Type t) { return newExpr2(t, NULL, NULL); }


static inline int to_number(ExprP e) {
	int result = (e->type == Num) ? e->numeric_arg1 : -1;
	return result;
}

ExprP make_church_char(int ch);

void setup_state(void) {
	// Set next_alloc to be the start of the constants region so we
	// allocate from there first.
	s->next_alloc = s->constants;
	s->cK = newExpr(K);
	s->cS = newExpr(S);
	s->cI = newExpr2(S2, s->cK, s->cK);
	s->KI = newExpr1(K1, s->cI);

	s->SI = newExpr1(S1, s->cI);
	s->KS = newExpr1(K1, s->cS);
	s->KK = newExpr1(K1, s->cK);
	s->SKSK = newExpr2(S2, s->KS, s->cK);

	s->cInc = newExpr(Inc);
	s->cZero = newExpr(Num);

	// Set up gc fields
	s->from_space_start = (ExprP)s->space1;
	s->from_space_end = (ExprP)(s->space1 + HEAP_SIZE);
	s->to_space_start = (ExprP)s->space2;
	s->to_space_end = (ExprP)(s->space2 + HEAP_SIZE);
	s->next_alloc = s->from_space_start;
	s->work_stack_top = (ExprP*)s->from_space_end;

	s->toplevel_root = &s->roots[0];
	s->church2int_root = &s->roots[1];
	s->root_stack_top = 2;

	// Preintialize the chuch numeral table
	for (unsigned i = 0; i <= 256; i++) {
		make_church_char(i);
	}
}


static inline bool in_arena(ExprP p) {
	return p >= s->from_space_start && p < s->from_space_end;
}

static inline void push_work(ExprP e) {
	*(--s->work_stack_top) = e;
}
static inline ExprP pop_work() {
	return *s->work_stack_top++;
}

static inline ExprP copy_object(ExprP obj) {
	//assert(obj != (Expr*)(-2));
	if (!in_arena(obj)) return obj;
	if (obj->forward) {
		//fprintf(stderr, "%p -> %p\n", obj, obj->forward);
		return obj->forward;
	}

	*s->next_alloc = *obj;
	//obj->type = (Type)1337;
	//obj->arg1 = obj->arg2 = (Expr*)(-2);

	push_work(s->next_alloc);
	obj->forward = s->next_alloc;
	//fprintf(stderr, "forwarding %p to %p\n", obj, obj->forward);
	return s->next_alloc++;
}

static void gc() {
	INC_COUNTER(gcs);
	// Set up next_alloc to point into the to-space
	s->next_alloc = s->to_space_start;
	s->work_stack_top = (ExprP *)s->to_space_end;

	// Process the roots
	for (int i = 0; i < s->root_stack_top; i++) {
		s->roots[i] = copy_object(s->roots[i]);
	}
	for (unsigned i = 0; i <= 256; i++) {
		s->cached_church_chars[i] = copy_object(s->cached_church_chars[i]);
	}

	while ((ExprP)s->work_stack_top != s->to_space_end) {
		//assert((ExprP)work_stack_top > next_alloc);
		ExprP cursor = pop_work();

		if (cursor->type != Num) {
			cursor->arg1 = copy_object(cursor->arg1);
			cursor->arg2 = copy_object(cursor->arg2);
		}
	}

	// Do the swap
	ExprP tmp = s->from_space_start;
	s->from_space_start = s->to_space_start;
	s->to_space_start = tmp;
	tmp = s->from_space_end;
	s->from_space_end = s->to_space_end;
	s->to_space_end = tmp;
}

static inline bool is_exhausted(int n) {
	return s->next_alloc + n >= s->from_space_end;
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

static inline void root(ExprP e) {
	s->roots[s->root_stack_top++] = e;
}
static inline ExprP unroot() {
	return s->roots[--s->root_stack_top];
}

static inline void check_rooted(int n, ExprP *e1, ExprP *e2) {
	if (is_exhausted(n)) {
		root(*e1);
		root(*e2);
		oom(n);
		*e2 = unroot();
		*e1 = unroot();
	}
}

static inline ExprP partial_apply(ExprP lhs, ExprP rhs) { // 1 alloc
	// You could do something more complicated here,
	// but I tried it and it didn't seem to improve
	// execution speed.
	return newExpr2(A, lhs, rhs);
}


ExprP make_church_char(int ch) {
	if (ch < 0 || ch > 256) {
		ch = 256;
	}

	if (s->cached_church_chars[ch] == 0) {
		if (ch == 0) {
			s->cached_church_chars[ch] = s->KI;
		} else if (ch == 1) {
			s->cached_church_chars[ch] = s->cI;
		} else {		
			s->cached_church_chars[ch] = newExpr2(S2, s->SKSK, make_church_char(ch-1));
		}
	}
	return s->cached_church_chars[ch];
}

static inline ExprP drop_i1(ExprP cur) {
	// Seperating out this into two checks gets a real speed win.
	// Presumably due to branch prediction.
	if (cur->type == I1) {
		do {
			cur = cur->arg1;
		} while (cur->type == I1);
	}
	return cur;
}

static ExprP partial_eval(ExprP node);

// This function modifies the object in-place so that
// all references to it see the new version.
// An additional root gets past in by reference so that we can root it
// if we need to. I don't really like it but it is fast.
static inline ExprP partial_eval_primitive_application(ExprP e, ExprP *prev) {
	INC_COUNTER(prim_apps);

	e->arg2 = drop_i1(e->arg2); // do it in place to free up space
	ExprP lhs = e->arg1, rhs = e->arg2;

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
		ExprP lhs = e->arg1;
		lhs->type = S2;
		lhs->arg1 = newExpr2(S2, s->cI, newExpr1(K1, make_church_char(getchar())));
		lhs->arg2 = newExpr1(K1, newExpr(LazyRead));
		// fall thru
	}
	case S2: // 2 allocs
	{
		check_rooted(2, &e, prev);
		//type = A; // XXX: Why is this OK?
		ExprP lhs = e->arg1, rhs = e->arg2;
		e->arg1 = partial_apply(lhs->arg1, rhs);
		e->arg2 = partial_apply(lhs->arg2, rhs);
		break;
	}
	case Inc: // 0 allocs - but recursion
	{
		// Inc is the one place we need to force evaluation of an rhs
		root(e);
		root(*prev);
		ExprP rhs_res = partial_eval(rhs);
		*prev = unroot();
		e = unroot();

		e->type = Num;
		e->numeric_arg1 = to_number(rhs_res) + 1;
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

	return e;
}

/*
ExprP Expr::partial_eval() {
	ExprP cur = this;
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
static ExprP partial_eval(ExprP node) {
	INC_COUNTER(part_apps);

	ExprP prev = 0;
	ExprP cur = node;
	for (;;) {
		cur = drop_i1(cur);
		// Chase down the left hand side (while building a list of
		// where we came from linked through arg1) until we find
		// something that isn't an application. Once we have that,
		// we can apply the primitive, and then repeat.
		while (cur->type == A) {
			ExprP next = drop_i1(cur->arg1);
			cur->arg1 = prev;
			prev = cur; cur = next;
		}
		if (!prev) { // we've gotten it down to something that isn't an application
			break;
		}
		ExprP next = cur; cur = prev;
		prev = cur->arg1;
		cur->arg1 = next;

		cur = partial_eval_primitive_application(cur, &prev);
	}

	return cur;
}


ExprP parse_expr(FILE* f) {
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
		ExprP p = parse_expr(f);
		ExprP q = parse_expr(f);
		return partial_apply(p, q);
	}
	case 'k': case 'K':
		return s->cK;
	case 's': case 'S':
		return s->cS;
	case 'i': case 'I':
		return s->cI;
	default:
		printf("Invalid character!\n");
		exit(1);
	}
	return 0;
}

ExprP parse_expr_top(FILE* f) {
	ExprP e = parse_expr(f);
	if (fgetc(f) != '\n') {
		fprintf(stderr, "input program missing trailing newline\n");
		exit(1);
	}
	return e;
}

static ExprP car(ExprP list) {
	return partial_apply(list, s->cK);
}

static ExprP cdr(ExprP list) {
	return partial_apply(list, s->KI);
}

static int church2int(ExprP church) {
	check(2);
	ExprP e = partial_apply(partial_apply(church, s->cInc), s->cZero);
	*s->church2int_root = e;
	int result = to_number(partial_eval(e));
	if (result == -1) {
		fputs("Runtime error: invalid output format (result was not a number)\n", stderr);
		exit(3);
	}
	*s->church2int_root = 0;
	return result;
}


int main(int argc, char** argv) {
	setup_state();
	
	FILE *f = stdin;
	if (argc == 2) {
		f = fopen(argv[1], "r");
		if (!f) {
			fprintf(stderr, "Unable to open the file \"%s\".\n", argv[1]);
			exit(1);
		}
	}
	ExprP e = parse_expr_top(f);
	*s->toplevel_root = partial_apply(e, newExpr(LazyRead));

	for (;;) {
		int ch = church2int(car(*s->toplevel_root));
		if (ch >= 256) {
#if DEBUG_COUNTERS
			fprintf(stderr, "     gcs: %d\n    news: %d\n", gcs, news);
			fprintf(stderr, "primapps: %d\npartapps: %d\n", prim_apps, part_apps);
#endif
			return ch-256;
		}
		putchar(ch);
		fflush(stdout);
		*s->toplevel_root = cdr(*s->toplevel_root);
	}
}
