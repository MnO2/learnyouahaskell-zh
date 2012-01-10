$: << File.dirname(__FILE__)

require 'util.rb'
require 'parser.rb'
require 'dom.rb'
require 'generators/to_html.rb'

module FD
    class FDoc < Node

        def anchor
            self.h1[0].anchor rescue ''
        end
        def title
            self.h1[0].title rescue ''
        end
        def sub_titles
            self.h2.map{|n| n.title } rescue []
        end
        
        def FDoc.parse(content)
            FDoc.new << FD::Parser.parse_block(content)
        end

        ####
        #private
        ####
        def h1
            self['h'].select{|n| n.rank==1}
        end
        def h2
            self['h'].select{|n| n.rank==2}
        end
    end
end
