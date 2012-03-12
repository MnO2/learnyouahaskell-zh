require 'util.rb'
require 'dom.rb'
require 'generators/to_html.rb'

module FD
    
    class Parser     
        class << self
        # inline 
        # *bold*, _italic_, `code`, {{{code}}}
        # [url] http://rawurl.orz [test.jpg]
        #
        # regex rules !!
        #
        # return a List of Nodes
        def parse_line(content)
            # returns an array [Node]
            def do_parse_line(str, content)
                if content=='' or content.nil?
                    return [T.new(str)] if str!=''
                    return []
                end
                
                new_node=
                    case content
                    # `` code `` need NOT to be parsed
                    when /^``(.*?)``/
                        ICode.new( $1 )
                    # also `ignore`
                    when /^`([^`]*?)`/
                        T.new( $1 )
                    # blah **highlight text** blah
                    when /^\*\*(.*?)\*\*/ 
                        Strong.new( parse_line($1) )
                    # blah *bold text* blah
                    when /^\*(.*?)\*/ 
                        B.new( parse_line($1) )
                    # blah _italic text_ blah
                    when /^_(.*?)_/                            
                        I.new( parse_line($1) )
                    #[a.jpg] blah
                    when /^\[\s*(?:\$|\^)?([^\s]*?\.(?:jpg|jpeg|gif|png|bmp))\s*\]/
                        Img.new( $1 )
                    # [url blah]
                    when /^\[\s*([^\s ]+?)\s*\]/ 
                        A.new( $1, $1 )
                    when /^\[(\s*[^\s]+?)[\s ]+(.*)\]/
                        nodes=parse_line( $2 )
                        A.new( $1 ) { nodes }
                    else
                        nil
                    end
                if new_node
                    result=[]
                    result << T.new(str) if str!=''
                    result << new_node
                    result += do_parse_line('', $')
                    return result
                end

                return do_parse_line(str+content.head, content.tail)
            end
            do_parse_line('', content)
        end

        # block
        # match paragraphs, =headings, lists, code 
        # \A required
        def parse_block(content)
            def do_parse(content)
                # nil or empty
                return [] if content.nil? or content.strip==''

                # match code
                # content inside this block should not be influenced by others like == orz ==
                # {{
                #   == blah ==
                #   code
                # }}
                if content=~/\A\{\{(.*?)\}\}[\s ]*$/m
                    return [Code.new( $1.strip )] + do_parse($') 
                end

                # match headers
                # = H1
                # = H1 =
                # == H2
                if content=~/\A(=+)[\s ]*([^=#\n]*)(?:#([^\n]*))?/
                    #puts content[0..10];exit;
                    rest=$'
                    rank=$1.length
                    title=$2.strip
                    anchor=$3.strip.gsub(/[\t\s ]/, '-') rescue title
                    return [H.new(rank, title, anchor)] + do_parse(rest)
                end

                # TODO -------------
                #   hr
                
                # para
                #     # item1
                #         # ii1
                #         # ii2
                #     # item2
                #
                #     /\A {4,}(#([^\n]*\n))+/m
                if content=~/\A( {4,}(# [^\n]*\n*))(^ {4,}([^\n]*\n*))*/m
                    str =$&;
                    rest=$';
                    #lines=str.split("\n").map{|l| l=~/ {4,}(?:#\s*)(.*)/; $1; }
                    #p str.split(/^ {4,}#/); exit;
                    items=str.split(/^ {4,7}#\s*/)[1..-1].map do |i| 
                        i.gsub!(/^ {4}/, '')
                        Li.new( do_parse(i) ) 
                    end
                    return [List.new( items )] + do_parse(rest)
                end

                # para
                #     * item1
                #         * ii1
                #         * ii2
                #     * item2
                # space required
                #
                # ctrl + c & ctrl + v rules
                if content=~/\A( {4,}(\* [^\n]*\n*))(^ {4,}([^\n]*\n*))*/m
                    str =$&;
                    rest=$';
                    items=str.split(/^ {4,7}\*\s*/)[1..-1].map do |i| 
                        i.gsub!(/^ {4}/, '')
                        Li.new( do_parse(i) ) 
                    end
                    return [OList.new( items )] + do_parse(rest)
                end

                # match quotes
                # p1
                #     quote
                #         sub quote
                #     quote2
                if content=~/\A( {4,7}([^\n]*)\n*)+/m
                    str  =$&;
                    rest =$';
                    quote = str.split("\n").map{|l| l=~/ {4}(.*)/; $1; }.join("\n")
                    return [Quote.new( do_parse(quote) )] + do_parse(rest)
                end


                # match the para image
                # [.img.jpg]
                # [$img.jpg] para content
                case content 
                    when /\A\s*\[\s*?\$\s*([^\s]*?\.(?:jpg|jpeg|gif|png|bmp))\s*?\]\s*\n/
                        return [Pic.new( $1, :right )] + do_parse($')
                    when /\A\s*\[\s*?\^\s*([^\s]*?\.(?:jpg|jpeg|gif|png|bmp))\s*?\]\s*\n/
                        return [Pic.new( $1, :left )] + do_parse($')
                    when /\A\s*\[\s*?\s*([^\s]*?\.(?:jpg|jpeg|gif|png|bmp))\s*?\]\s*\n/
                        return [Pic.new( $1 )] + do_parse($')
                    when /\A\s*\[\s*?\$\s*([^\s]*?\.(?:jpg|jpeg|gif|png|bmp))\s*?\]\s*(.*)\n?/
                        return [Pic.new( $1, :right ), P.new( parse_line($2) )] + do_parse($')
                    when /\A\s*\[\s*?\^\s*([^\s]*?\.(?:jpg|jpeg|gif|png|bmp))\s*?\]\s*(.*)\n?/
                        return [Pic.new( $1, :left ), P.new( parse_line($2) ) ] + do_parse($')
                    when /\A\s*\[\s*?\s*([^\s]*?\.(?:jpg|jpeg|gif|png|bmp))\s*?\]\s*(.*)\n?/
                        return [Pic.new( $1 ), P.new( parse_line($2) ) ] + do_parse($')
                end

                # __
                # 2 _ and more~
                # just a blank para
                if content=~/\A(__+)[ \t]*\n?/
                    return [P.new('')] + do_parse($')
                end

                # all else
                # at least it matches this
                #
                # empty lines are ignored
                content=~/\A([^\n]*)\n?/
                return do_parse($') if $1.strip==''
                return [P.new( parse_line($1) )] + do_parse($')
            end
            return Node.new( do_parse(content) )
        end
        end
    end
end
