#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-

$: << 'tools/fdoc/src/'
require 'fdoc.rb'
require 'erb'
require 'ropencc'

include FD

source_dir = 'source/_posts'
public_dir = 'public'
target_dir = ["zh-tw", "zh-cn"]


target_dir.each do |zh|
    Dir.chdir(source_dir) 
    files=Dir.glob('*.txt').sort.select{|fn| fn=~/^\d.*.txt/}.map{|fn| open(fn) }
    Dir.chdir('../../')

    docs=[];
    c=nil; p=nil; n=nil; 
    tpl=ERB.new( open('_page.erb').read )
    for i in 0..files.length-1
        p=c
        c=n || FDoc.parse( (zh == "zh-cn") ? Ropencc.conv('trad_to_simp', files[i].read) : files[i].read )
        docs << c
        n = FDoc.parse( (zh == "zh-cn") ? Ropencc.conv('trad_to_simp', files[i+1].read) : files[i+1].read ) rescue nil

        html=tpl.result(binding)

        fn="#{c.anchor.chomp('-')}.html"
        open("#{public_dir}/#{zh}/"+fn,'w'){|f| 
            f.write( html ) 
        }
        puts "#{public_dir}/#{zh}/#{fn}... \t#{File.size("#{public_dir}/#{zh}/"+fn).div(1024).to_s}kb"
    end

    c=Node.new {
      olist{
        docs.each do |d|
          li { 
            a "#{d.anchor.chomp('-')}.html", d.title 
            list {
              d.h2.each do |s|
                li { a "#{d.anchor.chomp('-')}.html\##{s.anchor.chomp('-').gsub /\s/, '_'}", s.title } 
              end
            }
          }
        end
      }
    }

    tpl = ERB.new( open('_chapters.erb').read )
    html = tpl.result(binding)

    f = open("#{public_dir}/#{zh}/chapters.html", 'w')
    f.write(html)
    f.close
    puts "#{public_dir}/#{zh}/chapters.html"
end

#parse faq
fn="faq.html"
c=FDoc.parse( open("#{source_dir}/faq.txt").read )
tpl=ERB.new( open('_faq.erb').read )
html=tpl.result(binding)
open("#{public_dir}/#{fn}", 'w'){|f|
    f.write( html )
}
puts "#{public_dir}/#{fn}... \t#{File.size("#{public_dir}/"+fn).div(1024).to_s}kb"

#parse resource
fn="resource.html"
c=FDoc.parse( open("#{source_dir}/resource.txt").read )
tpl=ERB.new( open('_faq.erb').read )
html=tpl.result(binding)
open("#{public_dir}/#{fn}", 'w'){|f|
    f.write( html )
}
puts "#{public_dir}/#{fn}... \t#{File.size("#{public_dir}/"+fn).div(1024).to_s}kb"
