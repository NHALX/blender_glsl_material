#lang reader "../../CSPL15/CSPL15.rkt"
(require
 (for-syntax racket/base
             racket/list
             racket/function
             syntax/parse))

(include: "OpenSceneGraph.hh")

(typedef: "osg::Matrix*" "m44_t")
(typedef: "osg::Vec4*"   "v4_t")
(typedef: "osg::Vec3*"   "v3_t")
(typedef: "float"        "real")

(include: "generated/scheme.h")

           
(ƒ: sync_matrix (matrix)
  ∷ "const osg::Matrix&" → "osg::Matrix"
  { return transpose(matrix); })


(ƒ: transpose (matrix)
  ∷ "const osg::Matrix &" → "osg::Matrix"
  { osg::Matrix m;
    for (int c = 0; c < 4; c++)
        for (int r = 0; r < 4; r++)
            m(c,r) = matrix(r,c);

    return m; })

(ƒ: normal_matrix (model_view_inv)
  ∷ "const osg::Matrix &" → "osg::Matrix3"
  { osg::Matrixd norm4;
    osg::Matrix3 norm3;

    norm4 = transpose(model_view_inv);
    norm3 = osg::Matrix3(norm4(0,0), norm4(0,1), norm4(0,2),
                         norm4(1,0), norm4(1,1), norm4(1,2),
                         norm4(2,0), norm4(2,1), norm4(2,2));
    return norm3; })


(include: "generated/linear-algebra.h")

(ƒ: m44mul (sl a b)
  ∷ (C-link) ⇒ "void *" → "const m44_t" → "const m44_t" → "const m44_t"
  { m44_t result = new osg::Matrix;
    result->set((*a) * (*b));
    return result; })


(ƒ: m44_invert (sl src)
  ∷ (C-link) ⇒ "void *" → "const m44_t" → "const m44_t"
  { m44_t result = new osg::Matrix;
    result->invert(*src);
    return result; })


(ƒ: m44_transpose (sl m0)
  ∷ (C-link) ⇒ "void *" → "const m44_t" → "const m44_t"
  { m44_t result = new osg::Matrix;
    result->set(transpose(*m0));
    return result; })


(ƒ: m44_col (sl m i)
  ∷ (C-link) ⇒ "void *" → "const m44_t" → "const ulong" → "const v4_t"
  { return new osg::Vec4((*m)(0,i),
                         (*m)(1,i),
                         (*m)(2,i),
                         (*m)(3,i)); })


(ƒ: m44mul_v3 (sl m_ v_)
  ∷ (C-link) ⇒ "void *" → "const m44_t" → "const v3_t" → "const v3_t"
  { osg::Vec4 temp = osg::Vec4(*v_,1.0);
    temp = (*m_) * temp;
    return new osg::Vec3(temp[0],temp[1],temp[2]); })


(ƒ: m33mul_v3 (sl m_ v_)
  ∷ (C-link) ⇒ "void *" → "const m44_t" → "const v3_t" → "const v3_t"
  { v3_t result = new osg::Vec3;
    result->set(osg::Matrix::transform3x3(*m_, *v_));
    return result; })


(ƒ: v4_v3 (sl v)
  ∷ (C-link) ⇒ "void *" → "const v3_t" → "const v4_t"
  { return
    new osg::Vec4((*v)[0], (*v)[1], (*v)[2], 1.0); })


(ƒ: v3_v4 (sl v)
  ∷ (C-link) ⇒ "void *" → "const v4_t" → "const v3_t"
  { return
    new osg::Vec3((*v)[0], (*v)[1], (*v)[2]); })


(ƒ: v3_negate (sl v1)
  ∷ (C-link) ⇒ "void *" → "const v3_t" → "const v3_t"
  { v3_t v2 = new osg::Vec3;
    v2->set(-(*v1));
    return v2; })


(ƒ: v3_normalize (sl v)
  ∷ (C-link) ⇒ "void *" → "const v3_t" → "const v3_t"
  { v3_t v2 = new osg::Vec3;
    v2->set(*v);
    v2->normalize();
    return v2; })



;; constructors

(define-for-syntax (generate-args w h)
  (define f
    (∘ flatten (⤶ build-list (* w h))))
  
  #`(#,(f (∘ string->symbol (⤶ format "x~a")))
     #,(f (const '(→ "const real")))))

(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name constructor result w h)
     (with-syntax [[((xs ...)
                     (ts ...))
                    (generate-args
                     (syntax->datum #'w)
                     (syntax->datum #'h))]]
       
       #`(ƒ: name (sl xs ...)
           ∷ (C-link) ⇒ "void *" ts ... → (format "const ~a" result)
           { return
             new @|constructor|(@|(join '(xs ...) ", ")|); }))]))



;; internal ops

(define-syntax-rule (define-internal-ops t internal-type vec-w vec-h print-code)
  
  (let [[tt       (format "~a_t" t)]
        [const-tt (format "const ~a_t" t)]]
    
   (begin
     
     (define-constructor t internal-type tt vec-w vec-h)
     
     (ƒ: (format "~a_eqf" t) (a b epsilon)
       ∷ (C-link) ⇒ const-tt → const-tt → "const real" → "boolean"
       { /* NOTE: 
          * - a ~= b, b ~= c, does not imply a ~= c
          * - does not handle NAN/INF etc */
      
         for (int i = 0; i < @|(* vec-w vec-h)|; ++i)
           if (fabs(a->ptr()[i] - b->ptr()[i]) > epsilon)
             return false;

         return true; })

     (ƒ: (format "_~a_t_equal" t) (v0 v1)
       ∷ (C-link) ⇒ const-tt → const-tt → "boolean"
         { return (*v0) == (*v1); })
     
     (ƒ: (format "_~a_t_free" t) (val)
       ∷ (C-link) ⇒ tt → "void"
         { delete val; })
     
     (ƒ: (format "_~a_t_show" t) (buf size v)
       ∷ (C-link) ⇒ "char *" → "size_t" → const-tt → "void"
         print-code))))



(define-internal-ops "v4" "osg::Vec4" 4 1
  { snprintf(buf, size,
            "(vec4 %.17g %.17g %.17g %.17g)\n", 
            (*v)[0],
            (*v)[1],
            (*v)[2],
            (*v)[3]); })

(define-internal-ops "v3" "osg::Vec3" 3 1
  { snprintf(buf, size,
             "(vec3 %.17g %.17g %.17g)\n", 
             (*v)[0],
             (*v)[1],
             (*v)[2]); })

(define-internal-ops "m44" "osg::Matrix" 4 4
  { snprintf(buf, size,
             "(m44 %.17g %.17g %.17g %.17g\n"
             "     %.17g %.17g %.17g %.17g\n"
             "     %.17g %.17g %.17g %.17g\n"
             "     %.17g %.17g %.17g %.17g)\n", 
             (*v)(0,0), (*v)(0,1), (*v)(0,2), (*v)(0,3),
             (*v)(1,0), (*v)(1,1), (*v)(1,2), (*v)(1,3),
             (*v)(2,0), (*v)(2,1), (*v)(2,2), (*v)(2,3),
             (*v)(3,0), (*v)(3,1), (*v)(3,2), (*v)(3,3)); })



;; ╻ ╻┏┓╻╻╺┳╸   ╺┳╸┏━╸┏━┓╺┳╸
;; ┃ ┃┃┗┫┃ ┃ ╺━╸ ┃ ┣╸ ┗━┓ ┃ 
;; ┗━┛╹ ╹╹ ╹     ╹ ┗━╸┗━┛ ╹

(include: <assert.h>)

(define (test type action target)
{{
    @|type| result   = @|action|;
    @|type| expected = @|target|;
    char buf[1024];

    _@|type|_show(buf, 1024, result);
    printf("@|action|:\nresult:\n%sexpected:\n@|target|\n\n", buf);

    assert(_@|type|_equal(result, expected));
    free(result);
    free(expected);
}})



(ƒ: unit_test_matrix () ∷ (C-link) ⇒ "int"
    
    { v4_t vec4 = v4(NULL,4,9,8,1);
      v3_t vec3 = v3(NULL,4,9,8);

      m44_t a = m44(NULL,
                    1, 2, 2, 10,
                    1, 1, 2, 4,
                    1, 2, 2, 4,
                    10, 2, 2, 4); }

    (test "m44_t" "m44mul(NULL, a, a)"
          "m44(NULL,105,28,30,66,44,15,16,38,45,16,18,42,54,34,36,132)")
    

    (test "m44_t" "m44_transpose(NULL, a)"
          "m44(NULL,1,1,1,10,2,1,2,2,2,2,2,2,10,4,4,4)")

    (test "v4_t" "m44_col(NULL, a, 0)"
          "v4(NULL,1,1,1,10)")

    (test "v3_t" "m44mul_v3(NULL, a, vec3)"
          "v3(NULL, 48, 33, 42)")

    (test "v3_t" "m33mul_v3(NULL, a, vec3)"
          "v3(NULL, 38, 29, 38)")

    (test "v4_t" "v4_v3(NULL, vec3)"
          "v4(NULL, 4, 9, 8, 1)")

    (test "v3_t" "v3_v4(NULL, vec4)"
          "v3(NULL, 4, 9, 8)")

    (test "v3_t" "v3_negate(NULL, vec3)"
          "v3(NULL, -4, -9, -8)")

    (test "v3_t" "v3_normalize(NULL, vec3)"
          "v3(NULL, 0.31524416249564025789301298748048,0.70929936561519058025927922183109, 0.63048832499128051578602597496097)")

   { return 0; })






