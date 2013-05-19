require 'util.rb'

# TODO : meta required 

module FD

    class Node
        # include Enumerable

        # attr
        attr                    :inner

        def initialize(*arg, &blc)
            @inner=[]
            
            if arg.size==1 and Array===arg[0] 
                nodes=arg[0]
                nodes.select{|i| Node===i }.each{|i| self << i}
            else
                self.init(*arg, &blc)
                r=instance_eval(&blc) if blc
                r.select{|i| Node===i }.each{|i| self << i } if Array===r && @inner==[]
            end
        end
        def init(*arg) end

        def << (node)
            @inner << node
            return self
        end
        alias add <<

        # TODO
        def [](t)
            arr=[]
            go_through{|i|
                arr<<i if (tstr(i)==t)
            }
            return arr
        end

        def ==(node)
            return (self.class==node.class) && (self.inner==node.inner)
        end
        def to_s(d=0)
            "#{'  '*d}#{self.class.to_s.split('::')[1].downcase} #{'"'+self.value.to_s+'"' rescue ''} \n#{self.inner.map{|i| i.to_s(d+1)}.join}"
        end

        # helper..
        def go_through(&blc)
            return if self.inner==[]
            self.inner.each{|i|
                yield i
                i.go_through(&blc)
            }
        end

        private
        def tstr(t)
            t.class.to_s.split('::').last.downcase
        end
    end


    #########################################
    # inline nodes 
    # they will displayed inside a P
    #########################################
    class B < Node
        def init(str=nil)
            self << T.new(str) if str
        end
    end

    class I < Node
        def init(str=nil)
            self << T.new(str) if str
        end
    end

    class Strong < Node
        def init(str=nil)
            self << T.new(str) if str
        end
    end

    class ICode < Node
        attr :value
        def init(str=nil)
            self << T.new(str) if str
        end
    end

    class A < Node
        attr :url
        def init(url, str=url, &blc)
            @url=url
            self << T.new(str) if str && !blc
        end
        def ==(node)
            super && self.url==node.url
        end
    end

    # atoms
    class T < Node
        attr :value
        def init(str)
            @value=str
        end
        def ==(other)
            super && self.value==other.value 
        end
    end

    class Img < Node
        attr :url
        def init(url)
            @url=url
        end
        def ==(other)
            super && self.url==other.url
        end
        def to_s(d)
            super
        end
    end

    ########################################
    # block
    # they will displayed as paragraphs...?
    ########################################

    # atoms first
    class H < Node
        attr :rank
        attr :value
        attr :anchor
        alias title value
        def init(rank, str, anchor=nil)
            @rank=rank
            @value=str
            @anchor=anchor || str
        end
        def ==(other)
            super && self.rank==other.rank && self.value==other.value && self.anchor==other.anchor
        end
    end

    class Pic < Node
        attr :url
        attr :float
        def init(url, float=:none)
            @url=url
            @float=float
        end
        def ==(other)
            super && self.url==other.url && self.float==other.float
        end
    end

    class Code < Node
        def init(str) 
            self << T.new(str) if str
        end
    end

    class P < Node
        def init(str=nil, &blc)
            self << T.new(str) if str
        end
    end

    class Quote < Node
        def init(str=nil, &blc)
            self << P.new(str) if str
        end
    end

    class List < Node
        def init(&blc)
        end
    end

    class OList < Node
        def init(&blc)
        end
    end

    class Li < Node
        def init(str=nil, &blc)
            self << P.new(str) if str
        end
    end
    
    #########################################################
    # for dsl 
    #########################################################
    # a ("http://swdpress.cn") { t "swdpress" }
    # p { 
    #   t "test"  
    #   b "orz"
    #   a "http://baidu.com" { iimg "test.jpg" "left" }
    # }
    #########################################################
    module NodeFactory
        # just test
        FD::constants.each {|name|
           klass=FD::const_get(name)
           if klass < FD::Node
               fname=name.downcase
               module_eval %{
                   def #{fname}(*args, &blc)
                       fr #{name}.new(*args, &blc)
                   end
               }
           end
        }
        def h1(*args, &blc)
            h(1, *args, &blc)
        end
        def h2(*args, &blc)
            h(2, *args, &blc)
        end
        def h3(*args, &blc)
            h(3, *args, &blc)
        end
        def fdoc(&blc)
            Node.new(&blc)
        end
        private
        def fr(n)
            self << n if self.is_a? Node
            return n
        end
    end
    include NodeFactory
    class Node
        include NodeFactory
    end
end 
