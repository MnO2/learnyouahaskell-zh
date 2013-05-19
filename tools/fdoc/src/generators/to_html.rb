module FD
    class Node
        def to_html
            self.content
        end
        def content
            str=(@inner || []).map{|i| i.to_html }.join || ''
            str=value.to_s rescue '' if str==''
            str
        end
    end

    ################################
    # inline nodes 
    class T
        def to_html
            def safe_html(s)
                s.gsub(/&(?!(#\d+|[a-zA-Z]+);)/, "&amp;").gsub(/</, "&lt;").gsub(/>/, "&gt;")
            end
            safe_html content
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
            "<a name=\"#{content.gsub /\s/, '_'}\"></a><h#{i}>#{content}</h#{i}>\n\n"
        end
    end

    class P
        def to_html
            "\n<p>#{content}</p>\n"
        end
    end

    class Pic
        def to_html
          "<img src=\"#{url}\" alt=\"\" style=\"float:#{float||'none'}\" class=\"float#{float||'none'}\" />"
        end
    end

    class Quote
        def to_html
            "<blockquote>#{content}</blockquote>\n"
        end
    end

    class Code
        def to_html
            "<pre class=\"code\">#{content}</pre>"
        end
    end

    class List
        def to_html
            "<ul>#{content}</ul>\n\n"
        end
    end

    class OList
        def to_html
            "<ol>#{content}</ol>\n\n"
        end
    end

    class Li
        def to_html
            "<li>#{content}</li>\n"
        end
    end
    
end
