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
