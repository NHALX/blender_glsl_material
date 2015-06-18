
class RenderTexture : public osg::Camera
{
private:
	osg::ref_ptr<osg::Texture2D> _texture;
    osg::Vec2                    _polyOffset;
    void applyDepthCullSettings();

public:
    enum BufferTarget {DEPTH,COLOR};
    RenderTexture(size_t size_x, size_t size_y, osg::Node *scene, BufferTarget type);
    
    osg::ref_ptr<osg::Texture2D> 
    getTexture()
    {
        return _texture;
    }
    
protected:
	RenderTexture();
};
