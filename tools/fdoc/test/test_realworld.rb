require 'util.rb'
require 'parser.rb'
require 'dom.rb'
require 'generators/to_html.rb'


class TestRealWorld < FdTestUtil
    
    def setup

    end
    
    def test_nop
        assert_equal(1, 1)
    end

    def test_txt1
        str=%{

it's a test text , it's *bold*, it's _it*al*ic_

para2

{{{
code here        
    if somthing is wrong
}}}

= h1
== h2 ==
=== h3

orz

para [www.baidu.com [test.jpg] ] 

[logo.jpg] para

[$ a.jpg] para
        }
        html = parse(str).to_html
        puts
        puts html
        #open('test.htm', 'w'){|f| f.write(html) }
    end

end


