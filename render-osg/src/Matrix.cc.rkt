#lang reader "../../misc/CSPL15.rkt"
| head
  #include "OpenSceneGraph.hh" |

@(c:typedef "osg::Matrix*" "m44_t")
@(c:typedef "osg::Vec4*"   "v4_t")
@(c:typedef "osg::Vec3*"   "v3_t")
@(c:typedef "float"        "real")

#include "generated/scheme.h"

@ƒ[
 sync_matrix ("" osg::Matrix ("const osg::Matrix&" matrix))
]{
  /*
    osg::Matrix m;
    m.set(matrix);
    return m; */
    return transpose(matrix);
}

@ƒ[
 transpose ("" osg::Matrix ("const osg::Matrix&" matrix))
]{
    osg::Matrix m;
    for (int c = 0; c < 4; c++)
        for (int r = 0; r < 4; r++)
            m(c,r) = matrix(r,c);

    return m;
}

@ƒ[
 normal_matrix ("" osg::Matrix3 ("osg::Matrix&" model_view_inv))
]{
    osg::Matrixd norm4;
    osg::Matrix3 norm3;

    norm4 = transpose(model_view_inv);
    norm3 = osg::Matrix3(norm4(0,0), norm4(0,1), norm4(0,2),
                         norm4(1,0), norm4(1,1), norm4(1,2),
                         norm4(2,0), norm4(2,1), norm4(2,2));
    return norm3;
}

//////////////////////////////////

extern "C"
{
    #include "generated/linear-algebra.h"

    const m44_t
    m44mul(void *sl, const m44_t a, const m44_t b)
    {
        m44_t result = new osg::Matrix;
        result->set((*a) * (*b));
        return result;
    }

    const m44_t
    m44_invert(void *sl, const m44_t src)
    {
        m44_t result = new osg::Matrix;
        result->invert(*src);
        return result;
    }


    const m44_t
    m44(void *sl, 
        const real x00, const real x01, const real x02, const real x03,
        const real x10, const real x11, const real x12, const real x13,
        const real x20, const real x21, const real x22, const real x23,
        const real x30, const real x31, const real x32, const real x33)
    {
        return new osg::Matrix(x00,x01,x02,x03,
                               x10,x11,x12,x13,
                               x20,x21,x22,x23,
                               x30,x31,x32,x33);
    }

    const m44_t
    m44_transpose(void *sl, const m44_t m0)
    {
        m44_t result = new osg::Matrix;
        result->set(transpose(*m0));
        return result;
    }

  /*
       (e-function "m44-frustum" "Construct a matrix that produces a perspective projection."
                (arg r real "left")
                (arg r real "right")
                (arg r real "bottom")
                (arg r real "top")
                (arg r real "znear")
                (arg r real "zfar")
                (arg r m44 ""))

    const m44_t
    m44_frustum(
        void *sl, 
        const real left,
        const real right,
        const real bottom,
        const real top,
        const real zNear,
        const real zFar)
    {
        m44_t m = new osg::Matrix;
        m->makeFrustum(left,right,bottom,top,zNear,zFar);
        // TODO: transpose?
        return m;
    }
    */

    const v4_t
    m44_col(void *sl, const m44_t m, const ulong i)
    {
        return new osg::Vec4((*m)(0,i),
                             (*m)(1,i),
                             (*m)(2,i),
                             (*m)(3,i));
    }


    const v3_t
    m44mul_v3(void *sl, const m44_t m_, const v3_t v_)
    {
        osg::Vec4 temp = osg::Vec4(*v_,1.0);
        temp = (*m_) * temp;
        return new osg::Vec3(temp[0],temp[1],temp[2]);
    }

    const v3_t
    m33mul_v3(void *sl, const m44_t m_, const v3_t v_)
    {
        v3_t result = new osg::Vec3;
        result->set(osg::Matrix::transform3x3(*m_, *v_));
        return result;
    }

    const v4_t
    v4(void *sl, const real x00, const real x01, const real x02, const real x03)
    {
        return new osg::Vec4(x00,x01,x02,x03);
    }

    const v4_t
    v4_v3(void *sl, v3_t v)
    {
        return new osg::Vec4((*v)[0], (*v)[1], (*v)[2], 1.0);
    }

    const v3_t
    v3_v4(void *sl, v4_t v)
    {
        return new osg::Vec3((*v)[0],(*v)[1],(*v)[2]);
    }

    const v3_t
    v3(void *sl, const real x00, const real x01, const real x02)
    {
        return new osg::Vec3(x00,x01,x02);
    }

    const v3_t
    v3_negate(void *sl, const v3_t v1)
    {
        v3_t v2 = new osg::Vec3;
        v2->set(-(*v1));
        return v2;
    }

    const v3_t
    v3_normalize(void *sl, const v3_t v)
    {
        v3_t v2 = new osg::Vec3;
        v2->set(*v);
        v2->normalize();
        return v2;
    }

    //////////////////
    @(define (approx-equal t n)
      @C{
        const boolean
        @|t|_eqf(const @|t|_t a, const @|t|_t b, real epsilon)
        {
            // NOTE: 
            // - a ~= b, b ~= c, does not imply a ~= c
            // - does not handle NAN/INF etc
            for (int i = 0; i < @|n|; ++i)
              if (fabs(a->ptr()[i] - b->ptr()[i]) > epsilon)
                  return false;

            return true;
        }

      })

    @(approx-equal "v4" 4)
    @(approx-equal "v3" 3)
    @(approx-equal "m44" 16)

    void
    _v4_t_show(char *buf, size_t size, const v4_t v)
    {    
        snprintf(buf, size,"(vec4 %.17g %.17g %.17g %.17g)\n", 
                 (*v)[0],(*v)[1],(*v)[2],(*v)[3]);
    }

    void
    _v3_t_show(char *buf, size_t size, const v3_t v)
    {    
        snprintf(buf, size,"(vec3 %.17g %.17g %.17g)\n", 
                 (*v)[0],(*v)[1],(*v)[2]);
    }

    void
    _m44_t_show(char *buf, size_t size, const m44_t v)
    {
        snprintf(buf, size,
                 "(m44 %.17g %.17g %.17g %.17g\n"
                 "     %.17g %.17g %.17g %.17g\n"
                 "     %.17g %.17g %.17g %.17g\n"
                 "     %.17g %.17g %.17g %.17g)\n", 
                 (*v)(0,0),(*v)(0,1),(*v)(0,2),(*v)(0,3),
                 (*v)(1,0),(*v)(1,1),(*v)(1,2),(*v)(1,3),
                 (*v)(2,0),(*v)(2,1),(*v)(2,2),(*v)(2,3),
                 (*v)(3,0),(*v)(3,1),(*v)(3,2),(*v)(3,3));
    }

    bool _v3_t_equal(v3_t v0, v3_t v1){ return (*v0) == (*v1); }
    bool _v4_t_equal(v4_t v0, v4_t v1){ return (*v0) == (*v1); }
    bool _m44_t_equal(m44_t v0, m44_t v1){ return (*v0) == (*v1); }

    void _m44_t_free(m44_t val){ delete val; }
    void _v4_t_free(v4_t val){ delete val; }
    void _v3_t_free(v3_t val){ delete val; }
}


// ╻ ╻┏┓╻╻╺┳╸   ╺┳╸┏━╸┏━┓╺┳╸
// ┃ ┃┃┗┫┃ ┃ ╺━╸ ┃ ┣╸ ┗━┓ ┃ 
// ┗━┛╹ ╹╹ ╹     ╹ ┗━╸┗━┛ ╹

#include <assert.h>

@(define (test type action target)
  @C{
    {
      @|type| result   = @|action|;
      @|type| expected = @|target|;
      char buf[1024];

      _@|type|_show(buf, 1024, result);
      printf("@|action|:\nresult:\n%sexpected:\n@|target|\n\n", buf);

      assert(_@|type|_equal(result, expected));
      free(result);
      free(expected);
    }
  })


int
unit_test_matrix()
{
    v4_t vec4 = v4(NULL,4,9,8,1);
    v3_t vec3 = v3(NULL,4,9,8);

    m44_t a = m44(NULL,
                  1, 2, 2, 10,
                  1, 1, 2, 4,
                  1, 2, 2, 4,
                  10, 2, 2, 4);  



    @(test "m44_t" "m44mul(NULL, a, a)"
           "m44(NULL,105,28,30,66,44,15,16,38,45,16,18,42,54,34,36,132)")

    /*
    @(test "m44_t" "m44_invert(NULL, a)"
           "m44(NULL,0.0,0.0,-0.111111,0.111111,0.0,-1.0,1.0,0.0,-0.333333,1.0, -0.111111,-0.0555556,0.166667,0.0,-0.166667,0.0)")

    */

    @(test "m44_t" "m44_transpose(NULL, a)"
           "m44(NULL,1,1,1,10,2,1,2,2,2,2,2,2,10,4,4,4)")

    @(test "v4_t" "m44_col(NULL, a, 0)"
           "v4(NULL,1,1,1,10)")

    @(test "v3_t" "m44mul_v3(NULL, a, vec3)"
           "v3(NULL, 48, 33, 42)")

    @(test "v3_t" "m33mul_v3(NULL, a, vec3)"
           "v3(NULL, 38, 29, 38)")

    @(test "v4_t" "v4_v3(NULL, vec3)"
           "v4(NULL, 4, 9, 8, 1)")

    @(test "v3_t" "v3_v4(NULL, vec4)"
           "v3(NULL, 4, 9, 8)")

    @(test "v3_t" "v3_negate(NULL, vec3)"
           "v3(NULL, -4, -9, -8)")

    @(test "v3_t" "v3_normalize(NULL, vec3)"
           "v3(NULL, 0.31524416249564025789301298748048,0.70929936561519058025927922183109, 0.63048832499128051578602597496097)")

    return 0;
}


}))


