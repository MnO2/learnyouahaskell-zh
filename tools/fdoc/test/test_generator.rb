require 'dom.rb'
require 'generators/to_html.rb'

include FD

class FdTestGenerator < FdTestUtil
    
    def test_bold
        a=B.new('test').to_html
        assert_equal('<b>test</b>', a)

        a=parse_line('*test*')[0].to_html
        assert_equal('<b>test</b>', a)
    end

    def test_italic
        a=I.new('test').to_html
        assert_equal('<i>test</i>', a)

        a=parse_line('_test_')[0].to_html
        assert_equal('<i>test</i>', a)
    end

    def test_image
        a=parse_line('[test.jpg]')[0].to_html
        assert_equal('<img src="test.jpg"></img>', a)
    end

    def test_link
        a=parse_line('[baidu.com]')[0].to_html
        assert_equal('<a href="baidu.com">baidu.com</a>', a)

        a=parse_line('[swdpress.cn ssword]')[0].to_html
        assert_equal('<a href="swdpress.cn">ssword</a>', a)
    end

    def test_para
    end

    def test_head
        a=h1("test").to_html
        assert_equal('<a name="test"></a><h1>test</h1>', a)
    end

    def test_para
        a=p{ b 'b'; t 'str'; }.to_html
        assert_equal("<p><b>b</b>str</p>", a)
    end

    def test_para_img
        a=Pic.new('test.jpg', :left).to_html
        assert_equal('<img src="test.jpg" style="float:left"></img>', a)
    end
    
    def test_code
        a=Code.new('test').to_html
        assert_equal('<pre class="code">test</pre>', a)
    end

    def test_list
        a=list{
            li "i1"
            li 'i2'
        }.to_html
        assert_equal('<ul><li><p>i1</p></li><li><p>i2</p></li></ul>', a)
    end

    def test_nlist
        a=olist{
            li 'i1'
            li 'i2'
        }.to_html
        assert_equal('<ol><li><p>i1</p></li><li><p>i2</p></li></ol>', a)
    end
end

