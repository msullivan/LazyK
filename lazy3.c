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
#include <stdlib.h>
#include <stdbool.h>

#if DEBUG_COUNTERS
#define INC_COUNTER(n) ((s->n)++)
#else
#define INC_COUNTER(n)
#endif

#define alloc(ty) calloc(sizeof(ty), 1)
#define alloc_array(ty,size) calloc(sizeof(ty), size)

#define A 1/*A*/
#define K 2/*K*/
#define K1 3/*K1*/
#define S 4/*S*/
#define S1 5/*S1*/
#define S2 6/*S2*/
#define I1 7/*I1*/
#define LazyRead 8/*LazyRead*/
#define Inc 9/*Inc*/
#define Num 10/*Num*/
typedef int Type;

typedef struct Expr Expr;
struct Expr {
	Expr *aux;
	Expr *arg1;
	Expr *arg2;
	int numeric_arg1;
	Type type;
};

typedef struct state_t {
	// Perf counters
	int news;
	int gcs;
	int prim_apps;
	int part_apps;

	// Garbage collection
	int HEAP_SIZE;
	Expr **space; //[HEAP_SIZE]
	int free_slots;
	Expr *next_alloc;
	Expr *work_stack_top;

	// Roots
	Expr **roots; // [MAX_ROOTS]
	int root_stack_top;
	Expr **cached_church_chars; //[257]

	// Preconstructed terms
	Expr *cK;
	Expr *cS;
	Expr *cI;
	Expr *KI;

	Expr *SI;
	Expr *KS;
	Expr *KK;
	Expr *SKSK;

	Expr *cInc;
	Expr *cZero;
} state;

Expr *alloc_expr(state *s) {
	INC_COUNTER(news);
	// We don't do an oom check. The caller better have already
	// done it with check or check_rooted.
	//if (free_slots == 0) {
	//	oom(1);
	//}
	Expr *expr = s->next_alloc;
	s->next_alloc = expr->aux;
	s->free_slots--;
	expr->aux = NULL;
	return expr;
}

Expr *newExpr2(state *s, Type t, Expr *a1, Expr *a2) {
	Expr *e = alloc_expr(s);
	e->aux = 0;
	e->type = t;
	e->arg1 = a1; e->arg2 = a2;
	e->numeric_arg1 = 0;
	return e;
}
Expr *newExpr1(state *s, Type t, Expr *a1) { return newExpr2(s, t, a1, NULL); }
Expr *newExpr(state *s, Type t) { return newExpr2(s, t, NULL, NULL); }


int to_number(Expr *e) {
	int result = (e->type == Num) ? e->numeric_arg1 : -1;
	return result;
}

Expr *make_church_char(state *s, int ch);

Expr *prepend(Expr *hd, Expr *tl) {
	hd->aux = tl;
	return hd;
}

int setup_state(state *s) {
	// Set up gc fields
	int MB = 1024*1024;
	int HEAP_SIZE_BYTES = 64*MB;
	int HEAP_SIZE = HEAP_SIZE_BYTES/sizeof(struct Expr);
	s->space = alloc_array(Expr *, HEAP_SIZE);
	// we need 2 roots for toplevel and church2int,
	// and then 2 per simultaneous invocation of partial_eval.
	// partial_eval only recurses as deep as the biggest number printed,
	// which can't /reasonably/ be above 512. This should be more than enough.
	int MAX_ROOTS = 1000;
	s->roots = alloc_array(Expr *, MAX_ROOTS);

	s->free_slots = HEAP_SIZE;
	s->HEAP_SIZE = HEAP_SIZE;
	Expr *hd = NULL;
	for (int i = 0; i < HEAP_SIZE; i++) {
		s->space[i] = alloc(Expr);
		hd = prepend(s->space[i], hd);
	}
	s->next_alloc = hd;
	s->work_stack_top = NULL;
	s->root_stack_top = 2; // 1 for toplevel, 1 for church2int

	// Set up constants
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

	// Preintialize the chuch numeral table
	s->cached_church_chars = alloc_array(Expr *, 257);
	for (unsigned i = 0; i <= 256; i++) {
		make_church_char(s, i);
	}

	return 0;
}

int push_work(state *s, Expr *e) {
	s->work_stack_top = prepend(e, s->work_stack_top);
	return 0;
}
Expr *pop_work(state *s) {
	Expr *expr = s->work_stack_top;
	s->work_stack_top = expr->aux;
	expr->aux = NULL;
	return expr;
}

int mark(state *s, Expr *e) {
	if (e == NULL) return 0;
	if ((int)e->type < 0) return 0;
	e->type = -e->type;
	push_work(s, e);
	return 0;
}

// Do a simple mark/sweep garbage collection over our heap 
int gc(state *s) {
	INC_COUNTER(gcs);
	// Set up next_alloc to point into the to-space
	s->next_alloc = NULL;
	s->free_slots = 0;
	assert(s->work_stack_top == NULL);

	// Process the roots
	for (int i = 0; i < s->root_stack_top; i++) {
		mark(s, s->roots[i]);
	}
	for (unsigned i = 0; i <= 256; i++) {
		mark(s, s->cached_church_chars[i]);
	}

	// Mark
	while (s->work_stack_top != NULL) {
		Expr *expr = pop_work(s);

		if (expr->type != Num) {
			mark(s, expr->arg1);
			mark(s, expr->arg2);
		}
	}

	// Sweep
	int HEAP_SIZE = s->HEAP_SIZE;
	Expr **space = s->space;
	for (int i = 0; i < HEAP_SIZE; i++) {
		Expr *e = space[i];
		if ((int)e->type < 0) { // Marked: clear the mark
			e->type = -e->type;
		} else { // Not marked: add to free list
			s->next_alloc = prepend(e, s->next_alloc);
			s->free_slots++;
		}
	}

	printf("gc done: reclaimed %d/%d\n", s->free_slots, HEAP_SIZE);
	return 0;
}

bool is_exhausted(state *s, int n) {
	return s->free_slots < n;
}

int oom(state *s, int n) {
	gc(s);
	if (is_exhausted(s, n)) {
		fprintf(stderr, "out of memory!\n");
		abort();
	}
	return 0;
}

int check(state *s, int n) {
	if (is_exhausted(s, n)) {
		oom(s, n);
	}
	return 0;
}

int root(state *s, Expr *e) {
	s->roots[s->root_stack_top] = e;
	s->root_stack_top++;
	return 0;
}
Expr *unroot(state *s) {
	--s->root_stack_top;
	return s->roots[s->root_stack_top];
}

int check_rooted(state *s, int n, Expr *e1, Expr *e2) {
	if (is_exhausted(s, n)) {
		root(s, e1);
		root(s, e2);
		oom(s, n);
		unroot(s);
		unroot(s);
	}
	return 0;
}

Expr *partial_apply(state *s, Expr *lhs, Expr *rhs) { // 1 alloc
	// You could do something more complicated here,
	// but I tried it and it didn't seem to improve
	// execution speed.
	return newExpr2(s, A, lhs, rhs);
}


Expr *make_church_char(state *s, int ch) {
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

Expr *drop_i1(Expr *cur) {
	// Seperating out this into two checks gets a real speed win.
	// Presumably due to branch prediction.
	if (cur->type == I1) {
		do {
			cur = cur->arg1;
		} while (cur->type == I1);
	}
	return cur;
}

Expr *partial_eval(state *s, Expr *node);

long int counts[15];
// This function modifies the object in-place so that
// all references to it see the new version.
// An additional root gets past in by reference so that we can root it
// if we need to. I don't really like it but it is fast.
Expr *partial_eval_primitive_application(state *s, Expr *e, Expr *prev) {
	INC_COUNTER(prim_apps);

	e->arg2 = drop_i1(e->arg2); // do it in place to free up space
	Expr *lhs = e->arg1;
	Expr *rhs = e->arg2;

	counts[lhs->type]++;

	// As an optimization, we sort the cases in order of frequency.
	int t = lhs->type;
	if (t == S2) { // 2 allocs
		check_rooted(s, 2, e, prev);
		//type = A; // XXX: Why is this OK?
		Expr *lhs = e->arg1;
		Expr *rhs = e->arg2;
		e->arg1 = partial_apply(s, lhs->arg1, rhs);
		e->arg2 = partial_apply(s, lhs->arg2, rhs);
	} else if (t == K1) { // 0 allocs
		e->type = I1;
		e->arg1 = lhs->arg1;
		e->arg2 = 0;
	} else if (t == K) { // 0 allocs
		e->type = K1;
		e->arg1 = rhs;
		e->arg2 = 0;
	} else if (t == S1) { // 0 allocs
		e->type = S2;
		e->arg1 = lhs->arg1;
		e->arg2 = rhs;
	} else if (t == S) { // 0 allocs
		e->type = S1;
		e->arg1 = rhs;
		e->arg2 = 0;
	} else if (t == Inc) { // 0 allocs - but recursion
		// Inc is the one place we need to force evaluation of an rhs
		root(s, e);
		root(s, prev);
		Expr *rhs_res = partial_eval(s, rhs);
		unroot(s);
		unroot(s);

		e->type = Num;
		e->numeric_arg1 = to_number(rhs_res) + 1;
		if (e->numeric_arg1 == 0) {
			fputs("Runtime error: invalid output format (attempted to apply inc to a non-number)\n",
			      stderr);
			abort();
		}
		e->arg2 = 0;
	} else if (t == LazyRead) { // 6 allocs (4+2 from S2)
		check_rooted(s, 6, e, prev);
		Expr *lhs = e->arg1;
		lhs->type = S2;
		lhs->arg1 = newExpr2(s, S2, s->cI, newExpr1(s, K1, make_church_char(s, getchar())));
		lhs->arg2 = newExpr1(s, K1, newExpr(s, LazyRead));

		// duplicate the S2 code
		check_rooted(s, 2, e, prev);
		//type = A; // XXX: Why is this OK?
		lhs = e->arg1;
		Expr *rhs = e->arg2;
		e->arg1 = partial_apply(s, lhs->arg1, rhs);
		e->arg2 = partial_apply(s, lhs->arg2, rhs);
	} else if (t == Num) {
		fputs("Runtime error: invalid output format (attempted to apply a number)\n", stderr);
		abort();
	} else {
		fprintf(stderr,
		        "INTERNAL ERROR: invalid type in partial_eval_primitive_application (%d)\n",
		        e->arg1->type);
		abort();
	}

	return e;
}

// evaluates until the toplevel thing is not a function application.
// a stack of nodes that are waiting for their first argument to be evaluated is built,
// chained through the first argument field
Expr *partial_eval(state *s, Expr *node) {
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

		cur = partial_eval_primitive_application(s, cur, prev);
	}

	return cur;
}


Expr *parse_expr(state *s) {
	int ch;

	// Wait until we get something we care about
	do {
		ch = getchar();
		if (ch == 35/*'#'*/) {
			while ((ch = getchar()) != 10/*'\n'*/)
				;
		}
	} while (ch == 10/*'\n'*/ || ch == 32/*' '*/);

	if (ch == 96/*'`'*/) {
		Expr *p = parse_expr(s);
		Expr *q = parse_expr(s);
		return partial_apply(s, p, q);
	} else if (ch == 107/*'k'*/) {
		return s->cK;
	} else if (ch == 115/*'s'*/) {
		return s->cS;
	} else if (ch == 105/*'i'*/) {
		return s->cI;
	} else {
		printf("Invalid character!\n");
		abort();
	}
	return NULL;
}

Expr *parse_expr_top(state *s) {
	Expr *e = parse_expr(s);
	if (getchar() != 10/*'\n'*/) {
		fprintf(stderr, "input program missing trailing newline\n");
		exit(1);
	}
	return e;
}

Expr *car(state *s, Expr *list) {
	return partial_apply(s, list, s->cK);
}

Expr *cdr(state *s, Expr *list) {
	return partial_apply(s, list, s->KI);
}

int church2int(state *s, Expr *church) {
	check(s, 2);
	Expr *e = partial_apply(s, partial_apply(s, church, s->cInc), s->cZero);
	s->roots[1] = e;
	int result = to_number(partial_eval(s, e));
	if (result == -1) {
		fputs("Runtime error: invalid output format (result was not a number)\n", stderr);
		abort();
	}
	s->roots[1] = NULL;
	return result;
}


int main(int argc, char** argv) {
	state *s = alloc(state);
	setup_state(s);
	
	Expr *e = parse_expr_top(s);
	s->roots[0] = partial_apply(s, e, newExpr(s, LazyRead));

	for (;;) {
		int ch = church2int(s, car(s, s->roots[0]));
		if (ch >= 256) {
#if DEBUG_COUNTERS
			fprintf(stderr, "     gcs: %d\n    news: %d\n", s->gcs, s->news);
			fprintf(stderr, "primapps: %d\npartapps: %d\n", s->prim_apps, s->part_apps);
#endif
			for (int i = A; i <= Num; i++) {
				printf("%2d: %ld\n", i, counts[i]);
			}
			return ch-256;
		}
		putchar(ch);
		fflush(stdout);
		s->roots[0] = cdr(s, s->roots[0]);
	}
}
