require 'test/unit'

$:.unshift "../src"
require 'parser.rb'


class FdTestInlineParser < FdTestUtil
    
    def setup
    end

    def test_basic
        #assert(parse_line('test'))
    end

    
    ###############################################################

    def test_normal
        assert(Array === parse_line('test'))
        #p parse_line('test')
        a=parse_line('test')
        x=fdoc {
            t 'test'
        }.inner
        assert_equal(x, a)
    end

    def test_ignore
        a=parse_line('`*ignore*`')
        x=fdoc{
            t '*ignore*' 
        }.inner
        assert_equal(x, a)
    end


    def test_bold
        assert(Array === parse_line('*test*'))
            
        a=parse_line('*bold text* normal')
        x=[(b 'bold text'), t(' normal')]
        assert_equal(x, a)

        a=parse_line('test*bold text* normal')
        x=[(t 'test'), (b 'bold text'), (t ' normal')]
        assert_equal(x, a)

        a=parse_line('*normal text')
        x=[t('*normal text')]
        assert_equal(x, a)
        a=parse_line('no*rmal text')
        x=[t('no*rmal text')]
        assert_equal(x, a)
    end

    def test_italic
        assert(Array === parse_line('test'))
        
        a=parse_line('_italic text_ normal')
        x=fdoc {
            i 'italic text'
            t ' normal'
        }.inner
        assert_equal(x, a)
        

        a=parse_line('_normal text')
        x=fdoc {
            t '_normal text'
        }.inner
        assert_equal(x, a)

        a=parse_line('no_rmal text')
        x=fdoc{
            t 'no_rmal text'
        }.inner
        assert_equal(x, a)
    end

    def test_bold_and_italic
        assert(Array === parse_line('_*test*_'))

        a=parse_line('_bigitalic *boldinside*_')
        x=fdoc{
            i {
                t 'bigitalic '
                b 'boldinside'
            }
        }.inner
        assert_equal(x, a)

        a=parse_line('_bigitalic*bold_*')
        x=fdoc{
            i 'bigitalic*bold' 
            t '*'
        }.inner
        assert_equal(x,a)
    end

    def test_code
        assert(Array === parse_line('`test`'))

        a=parse_line('``code``normal')
        x=fdoc{
            icode 'code'
            t 'normal'
        }.inner
        assert_equal(x, a)
    end

    def test_code2
        a=parse_line('``code``')
        assert(Array === a)

        x=fdoc{
            icode 'code'
        }.inner
        assert_equal(x, a)

        a=parse_line('````')
        x=fdoc {
            icode ''
        }.inner
        assert_equal(x, a)
        
        a=parse_line('``{``')
        x=fdoc {
            icode '{'
        }.inner
        assert_equal(x, a)

        a=parse_line('``a````b``')
        x=fdoc {
            icode 'a'
            icode 'b'
        }.inner
        assert_equal(x, a)
    end

    def test_image
        a=parse_line('[test.jpg]')
        x=fdoc {
            img 'test.jpg'
        }.inner
        assert_equal(x ,a)

        a=parse_line('[ test.jpg]')
        x=fdoc {
            img 'test.jpg'
        }.inner
        assert_equal(x, a)
        
        a=parse_line('[test.jpg ]')
        x=fdoc {
            img 'test.jpg'
        }.inner
        assert_equal(x, a)
    end

    def test_link
        a=parse_line('[http://swdpress.cn  test something]')
        x=[a('http://swdpress.cn', 'test something')]
        assert_equal(x, a)

        a=parse_line('[http://swdpress.cn]')
        x=[ a('http://swdpress.cn') ]
        assert_equal(x, a)

        a=parse_line('[ http://swdpress.cn]')
        x=[a('http://swdpress.cn')]
        assert_equal(x,a)

        a=parse_line('[http://swdpress.cn ]')
        x=[a('http://swdpress.cn')]
        assert_equal(x,a)

        a=parse_line('[baidu.com [test.jpg]]')
        x=[a('baidu.com'){ img 'test.jpg' }]
        assert_equal(x,a)

        a=parse_line('[baidu.com [swdpress.cn test]]')
        x=[a('baidu.com'){ a 'swdpress.cn', 'test' }]
        assert_equal(x, a)
    end

    ####################################################################3
    # test block!
    ####################################################################3
    # oui , we need heredoc(or what's the name?) here
    def test_normal
        a=parse('P').inner
        assert_equal([P.new('P')], a)
    end

    def test_Ps
        # blank lines are deserved to be ignored
        str="p1\np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new('p2')], a)


        str="\np1\np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new('p2')], a)

        str="p1\np2\n"
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new('p2')], a)

        str="p1\n\np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new('p2')], a)

        str="p1\n \np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new('p2')], a)

        str=%{
p1
 
p2
        }
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new('p2')], a)

        str=%{
p1
___
p2
        }
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new(''), P.new('p2')], a)

        str=%{
``p1``test
        }
        a=parse(str).inner
        x=fdoc{
            p {
                icode 'p1'
                t 'test'
            }
        }.inner
        assert_equal(x, a)

        str=%{
{{c1}}
``p1``test
        }
        a=parse(str).inner
        x=fdoc{
            code 'c1'
            p {
                icode 'p1'
                t 'test'
            }
        }.inner
        assert_equal(x, a)

        str=%{
。
   
在
        }
        a=parse(str).inner
        assert_equal([P.new('。'), P.new('在')], a)
    end

    def test_h
        str="=h1"
        a=parse(str).inner
        x=fdoc {h1 'h1'}.inner
        assert_equal(x, a)

        str="=h1"
        a=parse(str).inner
        x=fdoc {h1 'h1'}.inner
        assert_equal(x, a)

        str="==h2"
        a=parse(str).inner
        x=fdoc{h2 'h2'}.inner
        assert_equal(x,a)

        str="=h1\np1"
        a=parse(str).inner
        x=fdoc{
            h1 'h1'
            p 'p1'
        }.inner
        assert_equal(x,a)

        str="=h1\n==h2"
        a=parse(str).inner
        x=fdoc{
            h1 'h1'
            h2 'h2'
        }.inner
        assert_equal(x,a)

        str="=h1 #a1"
        a=parse(str).inner
        x=fdoc{h1 'h1', 'a1'}.inner
        assert_equal(x,a)
        x=fdoc{h1 'h1'}.inner
        assert_not_equal(x,a)
    end

    def test_code
        str="p1\n{{code}}\np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), Code.new('code'), P.new('p2')], a)

        str="p1\n{{co\nde}}\np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), Code.new("co\nde"), P.new('p2')], a)

        str="p1\np2\n{{co\nde}}\np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), P.new('p2'), Code.new("co\nde"), P.new('p2')], a)

        str="p1``code``p2"
        a=parse(str).inner
        x=fdoc{
            p {
                t 'p1'
                icode 'code'
                t 'p2'
            }
        }

        str="p1\n{{co{{{}}}de}}\np2"
        a=parse(str).inner
        assert_equal([P.new('p1'), Code.new("co{{{}}}de"), P.new('p2')], a)
    end

    def test_quote
        str=%{
p1
    q1}
        a=parse(str).inner
        x=fdoc{ p 'p1'; quote 'q1' }.inner
        assert_equal(x, a)

        str=%{
p1
    q1
p2
p3}
        a=parse(str).inner
        #assert_equal([P.new('p1'), Quote.new( P.new('q1') ), P.new('p2'), P.new('p3')], a)
        x=fdoc {
            p 'p1'
            quote 'q1'
            p 'p2'
            p 'p3'
        }.inner
        assert_equal(x, a)

        str=%{
p1
    q1
        subq1
    q2
p2}
        a=parse(str).inner
        x=fdoc{
            p 'p1'
            quote {
                p 'q1'
                quote 'subq1'
                p 'q2'
            }
            p 'p2'
        }.inner
        assert_equal(x,a)

        str=%{
p1
    q1p1
    q1p2}
        a=parse(str).inner
        assert_equal([  P.new('p1'),
                        Quote.new([
                            P.new('q1p1'),
                            P.new('q1p2')])], a)

        str=%{
p1
    q1

    q2}
        a=parse(str).inner
        assert_equal([  P.new('p1'),
                        Quote.new([
                            P.new('q1'),
                            P.new('q2')])], a)


        str=%{
p1
    ==q1h1
    q1}
        a=parse(str).inner
        x=fdoc{
            p 'p1'
            quote {
                h2 'q1h1'
                p 'q1'
            }
        }.inner
        assert_equal(x,a)
    end

    def test_list
        str=%{
p1
    # i1
    # i2
p2
        }
        a=parse(str).inner
        x=fdoc{
            p 'p1'
            list {
                li 'i1'
                li 'i2'
            }
            p 'p2'
        }.inner
        assert_equal(x,a)

        str=%{
p1
    # i1
        # ii1
        # ii2
    # i2
p2
        }
        a=parse(str).inner
        x=fdoc{
            p 'p1'
            list {
                li {
                    p 'i1'
                    list {
                        li 'ii1'
                        li 'ii2'
                    }
                }
                li 'i2'
            }
            p 'p2'
        }.inner
        assert_equal(x, a)

        str=%{
p
    # i1
    q1
    # i2
        }
        a=parse(str).inner
        assert_equal([
            P.new('p'),
            List.new([
                Li.new([
                    P.new('i1'),
                    P.new('q1')
                ]),
                Li.new('i2')
            ])
        ], a)

        str=%{
    # i1
    q1
    # i2
        }
        a=parse(str).inner
        assert_equal([
            List.new([
                Li.new([
                    P.new('i1'),
                    P.new('q1')
                ]),
                Li.new('i2')
            ])
        ], a)
    end

    def test_olist
        str=%{
p1
    * i1
    * i2
p2
        }
        a=parse(str).inner
        x=fdoc{
            p 'p1'
            olist {
                li 'i1'
                li 'i2'
            }
        }.inner
        assert_not_equal(x,a)
        x<< p('p2')
        assert_equal(x,a)

        str=%{
p1
    * i1
        * ii1
        * ii2
    * i2
p2
        }
        a=parse(str).inner
        assert_equal([  
            P.new('p1'),
            OList.new([
                Li.new([
                    P.new('i1'),
                    OList.new([
                        Li.new('ii1'),
                        Li.new('ii2')
                    ])
                ]),
                Li.new('i2')
            ]),
            P.new('p2')
        ], a)

        str=%{
p
    * i1
    q1
    * i2
        }
        a=parse(str).inner
        assert_equal([
            P.new('p'),
            OList.new([
                Li.new([
                    P.new('i1'),
                    P.new('q1')
                ]),
                Li.new('i2')
            ])
        ], a)

        str=%{
    * i1
    q1
    * i2
        }
        a=parse(str).inner
        assert_equal([
            OList.new([
                Li.new([
                    P.new('i1'),
                    P.new('q1')
                ]),
                Li.new('i2')
            ])
        ], a)
    end

    def test_list_and_nlist
        str=%{
    * ni1
        # i1
        # i2
    * ni2
        # i1
        }
        a=parse(str).inner
        x=fdoc{
            olist {
                li('ni1'){
                    list {
                        li 'i1'
                        li 'i2'
                    }
                }
                li('ni2'){
                    list {
                        li 'i1'
                    }
                }
            }
        }.inner
        assert_equal(x,a)

        str=%{
    # n1
        * n2
            # n3
        }
        a=parse(str).inner
        x=fdoc{
            list{
                li('n1'){
                    olist{
                        li('n2') {
                            list{
                                li 'n3'
                            }
                        }
                    }
                }   
            }
        }.inner

    end

    def test_p_image
        str=%{
[^a.jpg] P1
        }
        a=parse(str).inner
        x=[Pic.new('a.jpg', :left), P.new('P1')]
        assert_equal(x , a)

        str=%{
[$a.jpg] P
        }
        a=parse(str).inner
        assert_equal([Pic.new('a.jpg', :right), P.new('P')] , a)

        str=%{
p1
[$a.jpg] P
        }
        a=parse(str).inner
        assert_equal([P.new('p1'), Pic.new('a.jpg', :right), P.new('P')] , a)

        str=%{
[^a.jpg] [b.jpg] P
        }
        a=parse(str).inner
        x=fdoc{
            pic 'a.jpg', :left
            p { img 'b.jpg'; t ' P'}
        }.inner
        assert_equal(x,a)

    end

    <<-QUOTE 
    QUOTE

end
