package globals;

import util.KawaWrap;

public class WMatrix  extends Globals {
	/**
	 * Add methods related to matricies.
	 * 
	 * @param kawa
	 *            The interpreter to add them to.
	 * @throws Throwable
	 *             If anything breaks while adding them.
	 */
	public void addMethods(final KawaWrap kawa) throws Throwable {
		kawa.eval(
"(define (make-matrix i j)" +
"  (if (or (zero? i) (zero? j))" +
"      (make-vector 0)" +
"      (let ([mat (make-vector i)])" +
"        (let loop ([i (sub1 i)])" +
"          (if (negative? i)" +
"              mat" +
"              (begin" +
"                (vector-set! mat i (make-vector j 0))" +
"                (loop (sub1 i))))))))");
		
		kawa.eval("(define (matrix-rows mat) (vector-length mat))");
		kawa.eval("(define (matrix-cols mat) (if (zero? (matrix-rows mat)) 0 (vector-length (vector-ref mat 0))))");
		kawa.eval("(define (matrix-ref mat i j) (vector-ref (vector-ref mat i) j))");
		kawa.eval("(define (matrix-set! mat i j v) (vector-set! (vector-ref mat i) j v))");
		
		kawa.eval(
"(define (matrix-generator i j proc)" +
"  (let ([mat (make-matrix i j)])" +
"    (let loop ([r 0] [c 0])" +
"      (cond" +
"        [(= r i) mat]" +
"        [(= c j) (loop (add1 r) 0)]" +
"        [else" +
"         (matrix-set! mat r c (proc r c))" +
"         (loop r (add1 c))]))))");
		
		
	}
}
