#include "OpenSceneGraph.hh"
#include "Matrix.hh"

osg::Matrix transpose( const osg::Matrix& matrix ) 
{
    osg::Matrix m;
    for( int c = 0 ; c < 4 ; c++ ) 
        for( int r = 0 ; r < 4 ; r++ ) 
            m(c,r) = matrix(r,c);
    
    return m;
}

    
osg::Matrix3 normal_matrix(osg::Matrix &model_view_inv) 
{
    osg::Matrixd norm4;
    osg::Matrix3 norm3;
    
    norm4 = transpose(model_view_inv);
    norm3 = osg::Matrix3(norm4(0,0),norm4(0,1),norm4(0,2),
                         norm4(1,0),norm4(1,1),norm4(1,2),
                         norm4(2,0),norm4(2,1),norm4(2,2));
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
        return m;
    }


    const v4_t
    m44_col(void *sl, const m44_t m, const unsigned int i)
    {
        return new osg::Vec4((*m)(0,i),
                             (*m)(1,i),
                             (*m)(2,i),
                             (*m)(3,i));
    }


    const v3_t
    m44mul_v3(void *sl, const m44_t m_, const v3_t v_)
    {
        v3_t result = new osg::Vec3;
        result->set((*m_) * (*v_));
        return result;
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
    
    void
    _v4_show(char *buf, size_t size, const v4_t v)
    {    
        snprintf(buf, size,"(vec4 %f %f %f %f)\n", 
                 (*v)[0],(*v)[1],(*v)[2],(*v)[3]);
    }

    void
    _v3_show(char *buf, size_t size, const v3_t v)
    {    
        snprintf(buf, size,"(vec3 %f %f %f)\n", 
                 (*v)[0],(*v)[1],(*v)[2]);
    }
    
    void
    _m44_show(char *buf, size_t size, const m44_t v)
    {
        snprintf(buf, size,
                 "(m44 %f %f %f %f\n"
                 "     %f %f %f %f\n"
                 "     %f %f %f %f\n"
                 "     %f %f %f %f)\n", 
                 (*v)(0,0),(*v)(0,1),(*v)(0,2),(*v)(0,3),
                 (*v)(1,0),(*v)(1,1),(*v)(1,2),(*v)(1,3),
                 (*v)(2,0),(*v)(2,1),(*v)(2,2),(*v)(2,3),
                 (*v)(3,0),(*v)(3,1),(*v)(3,2),(*v)(3,3));
    }

    bool _v3_equal(v3_t v0, v3_t v1){ return (*v0) == (*v1); }
    bool _v4_equal(v4_t v0, v4_t v1){ return (*v0) == (*v1); }
    bool _m44_equal(m44_t v0, m44_t v1){ return (*v0) == (*v1); }

    void _m44_free(m44_t val){ delete val; }
    void _v4_free(v4_t val){ delete val; }
    void _v3_free(v3_t val){ delete val; }
}
