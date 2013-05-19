all:
	cp ../public/index.html .
	sed -i -e "s/<p class=\"langbox\">/<p class=\"langbox\" style=\"display: none;\">/;s/<div class=\"bs-docs-social\">/<div class=\"bs-docs-social\" style=\"display: none;\">/" ../public/index.html
	kindlegen book_tc.opf -o tmp.mobi
	kindlestrip.py tmp.mobi lyah_zh.mobi
	rm -f tmp.mobi
	mv index.html ../public/index.html

clean:
	rm -f *.mobi *.html

