// Lazy K interpreter in C0.
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

#if DEBUG_COUNTERS
#define INC_COUNTER(n) ((s->n)++)
#else
#define INC_COUNTER(n)
#endif
#define MAX_ROOTS 10000


struct Expr;
typedef struct Expr Expr;
typedef struct Expr * ExprP;
struct state_t;

void oom(struct state_t *s, int n);

typedef enum Type { A, K, K1, S, S1, S2, I1, LazyRead, Inc, Num, Free } Type;

struct Expr {
	ExprP forward;
	ExprP arg1;
	ExprP arg2;
	int numeric_arg1; // XXX
	Type type;
};
#define MB (1024*1024)
#define HEAP_SIZE_BYTES (64*MB)
#define HEAP_SIZE (HEAP_SIZE_BYTES/sizeof(struct Expr))

typedef struct state_t {
	// Perf counters
	int news;
	int gcs;
	int prim_apps;
	int part_apps;

	// Garbage collection
	Expr space1[HEAP_SIZE];
	Expr space2[HEAP_SIZE];
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
struct state_t main_state;

ExprP alloc_expr(state *s) {
	INC_COUNTER(news);
	// We don't do an oom check. The caller better have already
	// done it with check or check_rooted.
	//if (next_alloc >= from_space_end) {
	//	oom(1);
	//}
	ExprP expr = s->next_alloc;
	s->next_alloc++;
	return expr;
}

ExprP newExpr2(state *s, Type t, ExprP a1, ExprP a2) {
	ExprP e = alloc_expr(s);
	e->forward = 0;
	e->type = t;
	e->arg1 = a1; e->arg2 = a2;
	e->numeric_arg1 = 0;
	return e;
}
ExprP newExpr1(state *s, Type t, ExprP a1) { return newExpr2(s, t, a1, NULL); }
ExprP newExpr(state *s, Type t) { return newExpr2(s, t, NULL, NULL); }


int to_number(ExprP e) {
	int result = (e->type == Num) ? e->numeric_arg1 : -1;
	return result;
}

ExprP make_church_char(state *s, int ch);

void setup_state(state *s) {
	// Set next_alloc to be the start of the constants region so we
	// allocate from there first.
	s->next_alloc = s->constants;
	s->cK = newExpr(s, K);
	s->cS = newExpr(s, S);
	s->cI = newExpr2(s, S2, s->cK, s->cK);
	s->KI = newExpr1(s, K1, s->cI);

	s->SI = newExpr1(s, S1, s->cI);
	s->KS = newExpr1(s, K1, s->cS);
	s->KK = newExpr1(s, K1, s->cK);
	s->SKSK = newExpr2(s, S2, s->KS, s->cK);

	s->cInc = newExpr(s, Inc);
	s->cZero = newExpr(s, Num);

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
		make_church_char(s, i);
	}
}


bool in_arena(state *s, ExprP p) {
	return p >= s->from_space_start && p < s->from_space_end;
}

void push_work(state *s, ExprP e) {
	--s->work_stack_top;
	*s->work_stack_top = e;
}
ExprP pop_work(state *s) {
	return *s->work_stack_top++;
}

ExprP copy_object(state *s, ExprP obj) {
	//assert(obj != (Expr*)(-2));
	if (!in_arena(s, obj)) return obj;
	if (obj->forward) {
		//fprintf(stderr, "%p -> %p\n", obj, obj->forward);
		return obj->forward;
	}

	*s->next_alloc = *obj;
	//obj->type = (Type)1337;
	//obj->arg1 = obj->arg2 = (Expr*)(-2);

	push_work(s, s->next_alloc);
	obj->forward = s->next_alloc;
	//fprintf(stderr, "forwarding %p to %p\n", obj, obj->forward);
	return s->next_alloc++;
}

void gc(state *s) {
	INC_COUNTER(gcs);
	// Set up next_alloc to point into the to-space
	s->next_alloc = s->to_space_start;
	s->work_stack_top = (ExprP *)s->to_space_end;

	// Process the roots
	for (int i = 0; i < s->root_stack_top; i++) {
		s->roots[i] = copy_object(s, s->roots[i]);
	}
	for (unsigned i = 0; i <= 256; i++) {
		s->cached_church_chars[i] = copy_object(s, s->cached_church_chars[i]);
	}

	while ((ExprP)s->work_stack_top != s->to_space_end) {
		//assert((ExprP)work_stack_top > next_alloc);
		ExprP cursor = pop_work(s);

		if (cursor->type != Num) {
			cursor->arg1 = copy_object(s, cursor->arg1);
			cursor->arg2 = copy_object(s, cursor->arg2);
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

bool is_exhausted(state *s, int n) {
	return s->next_alloc + n >= s->from_space_end;
}

void oom(state *s, int n) {
	gc(s);
	if (is_exhausted(s, n)) {
		fprintf(stderr, "out of memory!\n");
		exit(4);
	}
}

void check(state *s, int n) {
	if (is_exhausted(s, n)) {
		oom(s, n);
	}
}

void root(state *s, ExprP e) {
	s->roots[s->root_stack_top++] = e;
}
ExprP unroot(state *s) {
	return s->roots[--s->root_stack_top];
}

void check_rooted(state *s, int n, ExprP *e1, ExprP *e2) {
	if (is_exhausted(s, n)) {
		root(s, *e1);
		root(s, *e2);
		oom(s, n);
		*e2 = unroot(s);
		*e1 = unroot(s);
	}
}

ExprP partial_apply(state *s, ExprP lhs, ExprP rhs) { // 1 alloc
	// You could do something more complicated here,
	// but I tried it and it didn't seem to improve
	// execution speed.
	return newExpr2(s, A, lhs, rhs);
}


ExprP make_church_char(state *s, int ch) {
	if (ch < 0 || ch > 256) {
		ch = 256;
	}

	if (s->cached_church_chars[ch] == 0) {
		if (ch == 0) {
			s->cached_church_chars[ch] = s->KI;
		} else if (ch == 1) {
			s->cached_church_chars[ch] = s->cI;
		} else {		
			s->cached_church_chars[ch] = newExpr2(s, S2, s->SKSK, make_church_char(s, ch-1));
		}
	}
	return s->cached_church_chars[ch];
}

ExprP drop_i1(ExprP cur) {
	// Seperating out this into two checks gets a real speed win.
	// Presumably due to branch prediction.
	if (cur->type == I1) {
		do {
			cur = cur->arg1;
		} while (cur->type == I1);
	}
	return cur;
}

ExprP partial_eval(state *s, ExprP node);

// This function modifies the object in-place so that
// all references to it see the new version.
// An additional root gets past in by reference so that we can root it
// if we need to. I don't really like it but it is fast.
ExprP partial_eval_primitive_application(state *s, ExprP e, ExprP *prev) {
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
		check_rooted(s, 6, &e, prev);
		ExprP lhs = e->arg1;
		lhs->type = S2;
		lhs->arg1 = newExpr2(s, S2, s->cI, newExpr1(s, K1, make_church_char(s, getchar())));
		lhs->arg2 = newExpr1(s, K1, newExpr(s, LazyRead));
		// fall thru
	}
	case S2: // 2 allocs
	{
		check_rooted(s, 2, &e, prev);
		//type = A; // XXX: Why is this OK?
		ExprP lhs = e->arg1, rhs = e->arg2;
		e->arg1 = partial_apply(s, lhs->arg1, rhs);
		e->arg2 = partial_apply(s, lhs->arg2, rhs);
		break;
	}
	case Inc: // 0 allocs - but recursion
	{
		// Inc is the one place we need to force evaluation of an rhs
		root(s, e);
		root(s, *prev);
		ExprP rhs_res = partial_eval(s, rhs);
		*prev = unroot(s);
		e = unroot(s);

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
ExprP partial_eval(state *s, ExprP node) {
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

		cur = partial_eval_primitive_application(s, cur, &prev);
	}

	return cur;
}


ExprP parse_expr(state *s, FILE* f) {
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
		ExprP p = parse_expr(s, f);
		ExprP q = parse_expr(s, f);
		return partial_apply(s, p, q);
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

ExprP parse_expr_top(state *s, FILE* f) {
	ExprP e = parse_expr(s, f);
	if (fgetc(f) != '\n') {
		fprintf(stderr, "input program missing trailing newline\n");
		exit(1);
	}
	return e;
}

ExprP car(state *s, ExprP list) {
	return partial_apply(s, list, s->cK);
}

ExprP cdr(state *s, ExprP list) {
	return partial_apply(s, list, s->KI);
}

int church2int(state *s, ExprP church) {
	check(s, 2);
	ExprP e = partial_apply(s, partial_apply(s, church, s->cInc), s->cZero);
	*s->church2int_root = e;
	int result = to_number(partial_eval(s, e));
	if (result == -1) {
		fputs("Runtime error: invalid output format (result was not a number)\n", stderr);
		exit(3);
	}
	*s->church2int_root = 0;
	return result;
}


int main(int argc, char** argv) {
	state *s = &main_state;
	setup_state(s);
	
	FILE *f = stdin;
	if (argc == 2) {
		f = fopen(argv[1], "r");
		if (!f) {
			fprintf(stderr, "Unable to open the file \"%s\".\n", argv[1]);
			exit(1);
		}
	}
	ExprP e = parse_expr_top(s, f);
	*s->toplevel_root = partial_apply(s, e, newExpr(s, LazyRead));

	for (;;) {
		int ch = church2int(s, car(s, *s->toplevel_root));
		if (ch >= 256) {
#if DEBUG_COUNTERS
			fprintf(stderr, "     gcs: %d\n    news: %d\n", s->gcs, s->news);
			fprintf(stderr, "primapps: %d\npartapps: %d\n", s->prim_apps, s->part_apps);
#endif
			return ch-256;
		}
		putchar(ch);
		fflush(stdout);
		*s->toplevel_root = cdr(s, *s->toplevel_root);
	}
}
