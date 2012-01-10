require 'fdoc.rb'

class TestFDoc < FdTestUtil
    
    def test_nop
        assert(1)
    end

    def test_parse
        str=%{
=test #header
==test2
        }
        doc=FDoc.parse(str)
    end

end


