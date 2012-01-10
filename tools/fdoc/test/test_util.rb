require 'util.rb'
require 'parser.rb'

class FdTestUtil < Test::Unit::TestCase

    def parse(content)
        FD::Parser.parse_block(content)
    end

    def parse_line(content)
        FD::Parser.parse_line(content)
    end

    def test_nop
        assert(1==1)
    end
    
end


