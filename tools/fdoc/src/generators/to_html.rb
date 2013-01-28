module FD
    class Node
        def to_html
            self.content
        end
        def content
            str=(@inner || []).map{|i| i.to_html }.join || ''
            str=value.to_s rescue '' if str==''
            str.strip
        end
    end

    ################################
    # inline nodes 
    class T
        def to_html
            content
        end
    end

    class B
        def to_html
            "<b>#{content}</b>"
        end
    end

    class Strong 
        def to_html
            "<strong>#{content}</strong>"
        end
    end

    class I
        def to_html
            "<i>#{content}</i>"
        end
    end

    class ICode
        def to_html
            "<code>#{content}</code>"
        end
    end

    class Img
        def to_html
            "<img src=\"#{url}\" alt=\"\"></img>"
        end
    end

    class A
        def to_html
            "<a href=\"#{url}\">#{content}</a>"
        end
    end

    # block
    class H
        def to_html
            i=self.rank
            "<a name=\"#{content.gsub /\s/, '_'}\"></a><h#{i}>#{content}</h#{i}>"
        end
    end

    class P
        def to_html
            "<p>#{content}</p>"
        end
    end

    class Pic
        def to_html
            "<img src=\"#{url}\" alt=\"\" style=\"float:#{float||'none'}\"></img>"
        end
    end

    class Quote
        def to_html
            "<blockquote>#{content}</blockquote>"
        end
    end

    class Code
        def to_html
            "<pre class=\"code\">#{content}</pre>"
        end
    end

    class List
        def to_html
            "<ul>#{content}</ul>"
        end
    end

    class OList
        def to_html
            "<ol>#{content}</ol>"
        end
    end

    class Li
        def to_html
            "<li>#{content}</li>"
        end
    end
    
end
