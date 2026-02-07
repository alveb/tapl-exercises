tru = lambda t. lambda f. t;
fls = lambda t. lambda f. f;
test = lambda l. lambda m. lambda n. l m n;
and = lambda b. lambda c. b c fls;
/* begin snippet 521 */
or = lambda b. lambda c. b tru c;
not = lambda b. b fls tru;
/* end snippet */
pair = lambda f. lambda s. lambda b. b f s;
fst = lambda p. p tru;
snd = lambda p. p fls;
c0 = lambda s. lambda z. z;
c1 = lambda s. lambda z. s z;
c2 = lambda s. lambda z. s (s z);
c3 = lambda s. lambda z. s (s (s z));
c4 = lambda s. lambda z. s (s (s (s z)));
suc = lambda n. lambda s. lambda z. s (n s z);
/* begin snippet 522 */
succ' = lambda n. lambda s. lambda z. n s (s z);
/* end snippet */
plus = lambda m. lambda n. lambda s. lambda z. m s (n s z);
/* end snippet */
times = lambda m. lambda n. m (plus n) c0;
/* begin snippet 523 */
times' = lambda m. lambda n. lambda s. lambda z. m (n s) z;
/* end snippet */
/* begin snippet 524 */
exp = lambda m. lambda n. n (times m) c1;
/* end snippet */
iszro = lambda n. n (lambda x. fls) tru;
zz = pair c0 c0;
ss = lambda p. pair (snd p) (plus c1 (snd p));
prd = lambda m. fst (m ss zz);
/* begin snippet 525 */
sub = lambda m. lambda n. n prd m;
/* end snippet */
/* begin snippet 527 */
equal = lambda m. lambda n. and (iszro (sub m n)) (iszro (sub n m));
/* end snippet */
/* begin snippet 528 */
/* [x, y, z] = lambda c. lambda n. c x (c y (c z n)); */
nil = lambda c. lambda n. n; /* Same as fls and c0. */
cons = lambda h. lambda t. lambda c. lambda n. c h (t c n);
head = lambda l. l tru fls;
tail = lambda l. fst (l
  (lambda h. lambda p. pair (snd p) (cons h (snd p)))
  (pair nil nil));
isnil = lambda l. l (lambda h. lambda t. fls) tru;
/* end snippet */
/*
 * l - list
 * c - cons
 * n - nil
 * a - cell (called h in prd)
 * g - generic function
 * h - head of list
 * t - tail of list
 */
tail' = lambda l. lambda c. lambda n.
  l (lambda h. lambda a. lambda g. g h (a c)) (lambda a. n)
  (lambda h. lambda t. t);
tail'' = lambda l. lambda c. lambda n.
  l (lambda h. lambda a. lambda g. g (a (c h))) (lambda a. n)
  (lambda t. t);
realbool = lambda b. b true false;
churchbool = lambda b. if b then tru else fls;
realeq = lambda m. lambda n. (equal m n) true false;
realnat = lambda m. m (lambda x. succ x) 0;
fix = lambda f.
  (lambda x. f (lambda y. x x y))
  (lambda x. f (lambda y. x x y));
factorial = fix (lambda factorial. lambda n.
  if realeq n c0 then c1 else (times n (factorial (prd n))));
/* begin snippet 529 */
factorial' = fix (lambda factorial. lambda n.
  test (equal n c0)
    (lambda _. c1)
    (lambda _. times n (factorial (prd n)))
  nil);
/* end snippet */
/* begin snippet 5210 */
churchnat = lambda n. lambda s. lambda z.
  fix (lambda f. lambda n. if iszero n then z else s (f (pred n))) n;
/* end snippet */
/* begin snippet 5211 */
sum = fix (lambda sum. lambda l.
  if realbool (isnil l) then c0
    else plus (head l) (sum (tail l)));
/* end snippet */
sum' = lambda l. l plus c0;
