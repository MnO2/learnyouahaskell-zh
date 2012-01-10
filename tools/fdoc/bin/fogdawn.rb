SRCROOT = 'd:\code\ruby\fogdawn\src'
$: << SRCROOT 

require 'fdoc.rb'
require 'erb'

include FD

plain_text = open(ARGV[0]).read rescue (puts("file error");exit;)
str_template = 
    if File.exist?(ARGV[1].to_s)
        open(ARGV[1]).read
    else
        open(SRCROOT+'/generators/default.erb').read
    end


doc=FDoc.parse( plain_text )
content=doc.to_html
template=ERB.new( str_template )

puts template.result(binding)


