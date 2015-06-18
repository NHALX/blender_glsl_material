
template <class node_t>
class FoldN : public osg::NodeVisitor 
{
    typedef void * (*fold_function)(void* x, node_t &);
    void *m_context;
    fold_function m_callback;

public:
    FoldN(fold_function f, void *ctx) : NodeVisitor( NodeVisitor::TRAVERSE_ALL_CHILDREN)
    {
        m_context  = ctx;
        m_callback = f;
    }

    void apply(node_t& n)
    {
        m_context = m_callback(m_context, n);
    }
};


template <class node_t>
class MapNode 
{   
public:
    typedef node_t* (*map_function_t)(node_t *);
    typedef std::list< osg::ref_ptr<node_t> > list_node_t;
    typedef list_node_t* list;

    static node_t* identity(node_t* n)
    {
        return n;
    }

    static list collect(osg::Node*node)
    {
        return map(identity, node);
    }
        
    static list map(map_function_t callback, osg::Node* node)
    {
        list xs = new list_node_t();
        return map(callback, node, xs);
    }
            
    static list map(map_function_t callback, osg::Node* node, list xs)
    {
       if (!node)
           return xs;

       node_t *x = dynamic_cast<node_t*>(node);
    
       if (x)
          xs->push_back(osg::ref_ptr<node_t>(callback(x)));
              
       osg::Group* group = node->asGroup(); 
       if (group) 
       {
          for (int i=0; i < group->getNumChildren(); i++)
              map(callback, group->getChild(i), xs);
       }

       return xs;
    }
};
/*
template <class node_t>
class MapNode : public osg::NodeVisitor 
{   
public:
    typedef node_t& (*map_function_t)(node_t &);
    std::list< osg::ref_ptr<node_t> > xs;
    map_function_t callback;

    void apply(osg::Node& n)
    {
        if (dynamic_cast<node_t*>(&n))
        {
                printf("hmmmmmmmmm!\n");
            apply(static_cast<node_t&>(n));
        }
        else
            osg::NodeVisitor::apply(n);
    }
        
    void apply(node_t& n)
    {
        xs.push_back(osg::ref_ptr<node_t>(&callback(n)));
    }
        
    MapNode(map_function_t(f)) : NodeVisitor(NodeVisitor::TRAVERSE_ALL_CHILDREN)
    {
        callback = f;
    }

    MapNode() : NodeVisitor(NodeVisitor::TRAVERSE_ALL_CHILDREN)
    {
        callback = identity;
    }
        
    static node_t& identity(node_t& n)
    {
        return n;
    }      
};
*/


class FoldGeodeGeometry : public osg::NodeVisitor 
{
    typedef void * (*fold_callback)(void* x, osg::Geode&, osg::Geometry*);
    void *m_context;
    fold_callback m_callback;
        
public: 
        
    FoldGeodeGeometry(fold_callback f, void *ctx) : NodeVisitor( NodeVisitor::TRAVERSE_ALL_CHILDREN )
    {
        m_context  = ctx;
        m_callback = f;
    }
        
    void apply(osg::Geode& geode)
    {        
        unsigned int numGeoms = geode.getNumDrawables();
 
        for (unsigned int geodeIdx = 0; geodeIdx < numGeoms; geodeIdx++ )
        { 
            osg::Geometry *g = geode.getDrawable(geodeIdx)->asGeometry();
            
            if (g)
                m_context = m_callback(m_context, geode, g);
        }
    }

};
