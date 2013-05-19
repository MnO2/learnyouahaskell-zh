/*
===============================================================================
Chili is the jQuery code highlighter plugin
...............................................................................
LICENSE: http://www.opensource.org/licenses/mit-license.php
WEBSITE: http://noteslog.com/chili/

											   Copyright 2008 / Andrea Ercolino
===============================================================================
*/

{
	  _name: "code"
	, _case: true
	, _main: {
		  mlcom  : { 
			  _match: /\/\*[^*]*\*+(?:[^\/][^*]*\*+)*\// 
			, _style: "color: #4040c2;"
		}
		, com    : { 
			  _match: /\/\/.*/ 
			, _style: "color: green;"
		}
		, string : { 
			  _match: /(?:\'[^\'\\\n]?(?:\\.[^\'\\\n]*)*\')|(?:\"[^\"\\\n]*(?:\\.[^\"\\\n]*)*\")/ 
			, _style: "color: #FF8080;"
		}
		, number : { 
			  _match: /(?:\b[+-]?(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][+-]?\d+)?\b)|(?:0x[a-f0-9]+)\b/ 
			, _style: "color: #FF9900;"
		}
        , hidden : {
            _match: /(?:ghci>)/
            , _style: "color: #ADBBAA;"
        }
        , op     : {
            _match: /(?:(`.*?`)|(=)|(==)|(>)|(>)|(>=)|(<=)|(\/=)|(\+)|(-)|(\*)|(\/)|(\+\+)|(&&)|(\|\|)|(>>)|(::)|(->)|(<-))/ 
            , _style: "color: #009999;"
        }
        , type   : {
            _match: /\b([A-Z][a-z]*)\b/
            , _style: "color: #93AA39;"
        }
		, keyword: { 
              _match: /\b(?:if|then|else|case|let|in|where|return|module|import|qualified|class|instance)\b/ 
			, _style: "color: #8989D9; font-weight: bold; "
    	}
	}
}
