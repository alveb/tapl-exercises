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
suc = lambda n. lambda s. lambda z. s (n s z);
/* begin snippet 522 */
succ' = lambda n. lambda s. lambda z. n s (s z);
/* end snippet */
plus = lambda m. lambda n. lambda s. lambda z. m s (n s z);
/* end snippet */
mult = lambda m. lambda n. m (plus n) c0;
/* begin snippet 523 */
mult' = lambda m. lambda n. lambda s. lambda z. m (n s) z;
/* end snippet */
/* begin snippet 524 */
exp = lambda m. lambda n. n (mult m) c1;
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
/* [a, b, c] = lambda f. lambda e. f a (f b (f c e)); */
nil = lambda f. lambda e. e; /* Same as fls and c0. */
cons = lambda h. lambda t. lambda f. lambda e. f h (t f e);
head = lambda l. l tru fls;
ee = pair nil nil;
ff = lambda h. lambda p. pair (snd p) (cons h (snd p));
tail = lambda l. fst (l ff ee);
/* end snippet */
fix = lambda f.
  (lambda x. f (lambda y. x x y))
  (lambda x. f (lambda y. x x y));
sum' = lambda l. l plus c0;
isnil = lambda l. l (lambda h. lambda t. fls) tru;
sum = fix (lambda sum. lambda l.
  if isnil l then c0 else plus (head l) (sum (tail l)));
