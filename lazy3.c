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

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define alloc(ty) calloc(sizeof(ty), 1)
#define alloc_array(ty,size) calloc(sizeof(ty), size)

int c(int ch) { putchar(ch); return 0; }
int num(int num) { printf("%d", num); return 0; }
int fail() { abort(); return 0; }

// Really, we want to be able to have:
// typedef enum Type { Free = 0, A = 1, K, K1, S, S1, S2, I1, LazyRead,
//                     Inc, Num } Type;
// But we don't get that in C0. If we ran the C preprocessor, that would
// also be much nicer. Instead we write things like S2/*6/. Sigh.
// N.B.: The type tags start at 1 so that we can negate the tags to
// mark them in the gc.

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

	Expr *KS;
	Expr *SKSK;

	Expr *cInc;
	Expr *cZero;
} state;

int debug_spew(state *s);
int root(state *s, Expr *e);

Expr *alloc_expr(state *s) {
	s->news++;
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
	int result = (e->type == 10/*Num*/) ? e->numeric_arg1 : -1;
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
	s->cK = newExpr(s, 2/*K*/);
	s->cS = newExpr(s, 4/*S*/);
	s->cI = newExpr2(s, 6/*S2*/, s->cK, s->cK);
	s->KI = newExpr1(s, 3/*K1*/, s->cI);

	s->KS = newExpr1(s, 3/*K1*/, s->cS);
	s->SKSK = newExpr2(s, 6/*S2*/, s->KS, s->cK);

	s->cInc = newExpr(s, 9/*Inc*/);
	s->cZero = newExpr(s, 10/*Num*/);

	// We need to root the constants or they might get GCd. Argh.
	root(s, s->cK); root(s, s->cS); root(s, s->cI); root(s, s->KI);
	root(s, s->KS); root(s, s->SKSK); root(s, s->cInc); root(s, s->cZero);

	// Preintialize the chuch numeral table
	s->cached_church_chars = alloc_array(Expr *, 257);
	for (int i = 0; i <= 256; i++) {
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
	if (e->type < 0) return 0;
	e->type = -e->type;
	push_work(s, e);
	return 0;
}

// Do a simple mark/sweep garbage collection over our heap 
int gc(state *s) {
	s->gcs++;
	// Set up next_alloc to point into the to-space
	s->next_alloc = NULL;
	s->free_slots = 0;

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

		if (expr->type != 10/*Num*/) {
			mark(s, expr->arg1);
			mark(s, expr->arg2);
		}
	}

	// Sweep
	int HEAP_SIZE = s->HEAP_SIZE;
	Expr **space = s->space;
	for (int i = 0; i < HEAP_SIZE; i++) {
		Expr *e = space[i];
		if (e->type < 0) { // Marked: clear the mark
			e->type = -e->type;
		} else { // Not marked: add to free list
			e->type = 0;
			s->next_alloc = prepend(e, s->next_alloc);
			s->free_slots++;
		}
	}

	//printf("gc done: reclaimed %d/%d\n", s->free_slots, HEAP_SIZE);
	return 0;
}

bool is_exhausted(state *s, int n) {
	return s->free_slots < n;
}

int oom(state *s, int n) {
	gc(s);
	if (is_exhausted(s, n)) {
		// "out of memory!\n"
		c(111);c(117);c(116);c(32);c(111);c(102);c(32);c(109);
		c(101);c(109);c(111);c(114);c(121);c(33);c(10);
		fail();
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
	return newExpr2(s, 1/*A*/, lhs, rhs);
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
			s->cached_church_chars[ch] = newExpr2(s, 6/*S2*/, s->SKSK,
			                                      make_church_char(s, ch-1));
		}
	}
	return s->cached_church_chars[ch];
}

Expr *drop_i1(Expr *cur) {
	// Seperating out this into two checks gets a real speed win.
	// Presumably due to branch prediction.
	if (cur->type == 7/*I1*/) {
		do {
			cur = cur->arg1;
		} while (cur->type == 7/*I1*/);
	}
	return cur;
}

Expr *partial_eval(state *s, Expr *node);

int print_runtime_error() {
	c(82);c(117);c(110);c(116);c(105);c(109);c(101);c(32);c(101);c(114);
	c(114);c(111);c(114);c(58);c(32);c(105);c(110);c(118);c(97);c(108);
	c(105);c(100);c(32);c(111);c(117);c(116);c(112);c(117);c(116);c(32);
	c(102);c(111);c(114);c(109);c(97);c(116);c(32);
	return 0;
}
int err_inc_non_num() {
	// "Runtime error: invalid output format (applied inc to a non-number)\n"
	print_runtime_error();
	c(40);c(97);c(112);c(112);c(108);c(105);c(101);c(100);c(32);c(105);
	c(110);c(99);c(32);c(116);c(111);c(32);c(97);c(32);c(110);c(111);
	c(110);c(45);c(110);c(117);c(109);c(98);c(101);c(114);c(41);c(10);
	return fail();
}
int err_apply_num() {
	//"Runtime error: invalid output format (attempted to apply a number)\n"
	print_runtime_error();
	c(40);c(97);c(116);c(116);c(101);c(109);c(112);c(116);c(101);c(100);
	c(32);c(116);c(111);c(32);c(97);c(112);c(112);c(108);c(121);c(32);
	c(97);c(32);c(110);c(117);c(109);c(98);c(101);c(114);c(41);c(10);
	return fail();
}
int err_not_num() {
	//"Runtime error: invalid output format (result was not a number)\n"
	print_runtime_error();
	c(40);c(114);c(101);c(115);c(117);c(108);c(116);c(32);c(119);c(97);
	c(115);c(32);c(110);c(111);c(116);c(32);c(97);c(32);c(110);c(117);
	c(109);c(98);c(101);c(114);c(41);c(10);
	return fail();
}
int err_invalid_type(Type t) {
	//"INTERNAL ERROR: invalid type in partial_eval_primitive_application (%d)\n"
	c(73);c(78);c(84);c(69);c(82);c(78);c(65);c(76);c(32);c(69);c(82);
	c(82);c(79);c(82);c(58);c(32);c(105);c(110);c(118);c(97);c(108);
	c(105);c(100);c(32);c(116);c(121);c(112);c(101);c(32);c(105);c(110);
	c(32);c(112);c(97);c(114);c(116);c(105);c(97);c(108);c(95);c(101);
	c(118);c(97);c(108);c(95);c(112);c(114);c(105);c(109);c(105);c(116);
	c(105);c(118);c(101);c(95);c(97);c(112);c(112);c(108);c(105);c(99);
	c(97);c(116);c(105);c(111);c(110);c(32);c(40);c(37);
	num(t);
	c(41);c(10);
	return fail();
}

// This function modifies the object in-place so that
// all references to it see the new version.
// An additional root gets past in by reference so that we can root it
// if we need to. I don't really like it but it is fast.
Expr *partial_eval_primitive_application(state *s, Expr *e, Expr *prev) {
	s->prim_apps++;

	e->arg2 = drop_i1(e->arg2); // do it in place to free up space
	Expr *lhs = e->arg1;
	Expr *rhs = e->arg2;

	// As an optimization, we sort the cases in order of frequency.
	Type t = lhs->type;
	if (t == 6/*S2*/) { // 2 allocs
		check_rooted(s, 2, e, prev);
		//e->type = 1/*A*/; // the type is already A
		Expr *lhs = e->arg1;
		Expr *rhs = e->arg2;
		e->arg1 = partial_apply(s, lhs->arg1, rhs);
		e->arg2 = partial_apply(s, lhs->arg2, rhs);
	} else if (t == 3/*K1*/) { // 0 allocs
		e->type = 7/*I1*/;
		e->arg1 = lhs->arg1;
		e->arg2 = NULL;
	} else if (t == 2/*K*/) { // 0 allocs
		e->type = 3/*K1*/;
		e->arg1 = rhs;
		e->arg2 = NULL;
	} else if (t == 5/*S1*/) { // 0 allocs
		e->type = 6/*S2*/;
		e->arg1 = lhs->arg1;
		e->arg2 = rhs;
	} else if (t == 4/*S*/) { // 0 allocs
		e->type = 5/*S1*/;
		e->arg1 = rhs;
		e->arg2 = NULL;
	} else if (t == 9/*Inc*/) { // 0 allocs - but recursion
		// Inc is the one place we need to force evaluation of an rhs
		root(s, e);
		root(s, prev);
		Expr *rhs_res = partial_eval(s, rhs);
		unroot(s);
		unroot(s);

		e->type = 10/*Num*/;
		e->numeric_arg1 = to_number(rhs_res) + 1;
		if (e->numeric_arg1 == 0) {
			err_inc_non_num();
		}
		e->arg1 = NULL;
		e->arg2 = NULL;
	} else if (t == 8/*LazyRead*/) { // 6 allocs
		check_rooted(s, 6, e, prev);
		Expr *lhs = e->arg1;
		lhs->type = 6/*S2*/;
		lhs->arg1 = newExpr2(s, 6/*S2*/, s->cI,
		                     newExpr1(s, 3/*K1*/,
							          make_church_char(s, do_read())));
		lhs->arg2 = newExpr1(s, 3/*K1*/, newExpr(s, 8/*LazyRead*/));

		// duplicate the S2 code
		//e->type = 1/*A*/; // the type is already A
		Expr *rhs = e->arg2;
		e->arg1 = partial_apply(s, lhs->arg1, rhs);
		e->arg2 = partial_apply(s, lhs->arg2, rhs);
	} else if (t == 10/*Num*/) {
		err_apply_num();
	} else {
		err_invalid_type(t);
	}

	return e;
}

// evaluates until the toplevel thing is not a function application.
// a stack of nodes that are waiting for their first argument to be
// evaluated is built, chained through the first argument field
Expr *partial_eval(state *s, Expr *node) {
	s->part_apps++;

	Expr *prev = 0;
	Expr *cur = node;
	for (;;) {
		cur = drop_i1(cur);
		// Chase down the left hand side (while building a list of
		// where we came from linked through arg1) until we find
		// something that isn't an application. Once we have that,
		// we can apply the primitive, and then repeat.
		while (cur->type == 1/*A*/) {
			Expr *next = drop_i1(cur->arg1);
			cur->arg1 = prev;
			prev = cur; cur = next;
		}
		if (!prev) {
			// we've gotten it down to something that isn't an application
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
		ch = do_read();
		if (ch == 35/*'#'*/) {
			while ((ch = do_read()) != 10/*'\n'*/)
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
		//"Invalid character!\n"
		c(73);c(110);c(118);c(97);c(108);c(105);c(100);c(32);c(99);c(104);
		c(97);c(114);c(97);c(99);c(116);c(101);c(114);c(33);c(10);
		fail();
	}
	return NULL;
}

Expr *parse_expr_top(state *s) {
	Expr *e = parse_expr(s);
	if (getchar() != 10/*'\n'*/) {
		//"input program missing trailing newline\n"
		c(105);c(110);c(112);c(117);c(116);c(32);c(112);c(114);c(111);c(103);
		c(114);c(97);c(109);c(32);c(109);c(105);c(115);c(115);c(105);c(110);
		c(103);c(32);c(116);c(114);c(97);c(105);c(108);c(105);c(110);c(103);
		c(32);c(110);c(101);c(119);c(108);c(105);c(110);c(101);c(10);
		fail();
	}
	return e;
}

Expr *car(state *s, Expr *list) {
	check(s, 1);
	return partial_apply(s, list, s->cK);
}

Expr *cdr(state *s, Expr *list) {
	check(s, 1);
	return partial_apply(s, list, s->KI);
}

int church2int(state *s, Expr *church) {
	s->roots[1] = church;
	check(s, 2);
	Expr *e = partial_apply(s, partial_apply(s, church, s->cInc), s->cZero);
	s->roots[1] = e;
	int result = to_number(partial_eval(s, e));
	if (result == -1) {
		err_not_num();
	}
	s->roots[1] = NULL;
	return result;
}

int debug_spew(state *s) {
	//printf("     gcs: %d\n    news: %d\n", s->gcs, s->news);
	//printf("primapps: %d\npartapps: %d\n", s->prim_apps, s->part_apps);
	c(32);c(32);c(32);c(32);c(32);c(103);c(99);c(115);c(58);c(32);
	num(s->gcs); c(10);
	c(32);c(32);c(32);c(32);c(110);c(101);c(119);c(115);c(58);c(32);
	num(s->news); c(10);
	c(112);c(114);c(105);c(109);c(97);c(112);c(112);c(115);c(58);c(32);
	num(s->prim_apps); c(10);
	c(112);c(97);c(114);c(116);c(97);c(112);c(112);c(115);c(58);c(32);
	num(s->part_apps); c(10);

	return 0;
}


int main(int argc, char** argv) {
	state *s = alloc(state);
	setup_state(s);
	
	Expr *e = parse_expr_top(s);
	s->roots[0] = partial_apply(s, e, newExpr(s, 8/*LazyRead*/));

	for (;;) {
		int ch = church2int(s, car(s, s->roots[0]));
		if (ch >= 256) {
			debug_spew(s);
			return ch-256;
		}
		putchar(ch);

		s->roots[0] = cdr(s, s->roots[0]);
	}
}
