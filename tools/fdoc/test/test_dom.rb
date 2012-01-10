require 'test/unit'

$: << "../src"
require 'dom.rb'
include FD


class FdTestDom < FdTestUtil
    
    def test_nop
        assert(1==1)
    end

    def test_node
        a = fdoc {
            p { 
                t "test" 
                t "para"
            }
        }
        assert_equal(1, a.inner.length)
        assert_instance_of(P, a.inner[0])
        assert_equal("test", a.inner[0].inner[0].value)
        assert_equal("para", a.inner[0].inner[1].value)

        a = Node.new {
            b "test"
        }
        assert_equal("test", a.inner[0].inner[0].value)

        a = fdoc {
            b("hello") { 
                t "world" 
            }
        }
        b= (B.new << T.new("hello") << T.new("world"))
        assert_equal(b, a.inner[0])
    end

    def test_meta
        a=fdoc{
            b 'test'
        }
        puts a['b']
        assert_equal(1, a['b'].size)

        assert_equal([], a['h'])
        a=fdoc{
            h1 'test'
        }
        puts a['h']
        assert_equal(1, a['h'].size)

        a=fdoc{
            p {
                t 't1'
            }
            p 't2'
            p 't3'
        }
        puts 
        puts a['t'].size
        assert_equal(3, a['t'].size)

        assert_kind_of(Node, a['t'][0])

        a=fdoc{
            h1 'h1'
            h2 'h21'
            h2 'h22'
        }
        assert_equal(3, a['h'].size)

        a=Node.new([h1('h1'),h1('h1`'),h2('h2')])
        assert_equal(3, a['h'].size)
    end

    def test_meta2
        str=%{
teest
==h2
teset
==h2`
test}
        puts '************************'
        a=FDoc.parse(str)
        puts a
        puts a['h']
        assert_equal(2, a['h'].size)
        
    end

    def test_root
        a=p "test"
        b=P.new(T.new("test"))
        assert_not_equal(b, a)
        b=P.new << T.new("test")
        assert_equal(b, a)
    end

    def test_equal
        a=t "test"
        b=t 'tes'
        assert_not_equal(b, a)
        b=t 'test'
        assert_equal(b, a)
    end
    def test_p_img
        a = p {
            img 'test.jpg'
            t 'ok?'
        }
        b=P.new << Img.new('test.jpg')
        assert_not_equal(b, a)
        b << T.new('ok?')
        assert_equal(b, a)
    end

    def test_a
        a= fdoc{
            a 'http://baidu.com', 'baidu'
        }.inner[0]
        x=A.new('http://baidu.com')
        assert_not_equal(x,a)
        x=A.new('http://baidu.com', 'baidu')
        assert_equal(x,a)
    end

    def test_list
        a = list{
            li 'item1'
            li 'item2'
        }
        b=List.new << Li.new('item1') 
        assert_not_equal(a, b)
        b << Li.new('item2')
        assert_equal(a, b)
    end
end

