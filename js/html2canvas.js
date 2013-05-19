/*
 * Copyright Niklas von Hertzen (hertzen.com)
 * This version of the script is not free for use, as it is still work-in-progress and I don't want to release any version of this 
 * in its current form yet. I will make this open-source and free for use once I feel it is in a state where it should be made public.
 */

(function($) {
    $.fn.html2canvas = function(options) {
        var opts = $.extend({
           
            logging:false,
            canvasLeft:0,
            canvasTop:0,
            ready: function(){}
		
        }, $.fn.html2canvas.defaults, options);
			
        var bodyLeft = parseInt($(this).css('margin-left'));
        var bodyTop = parseInt($(this).css('margin-top'));
        
        var canvas = $('<canvas />').attr('width',$(document).width()+100).attr('height',$(document).height()+100).css('position','absolute').css('left',opts.canvasLeft).css('top',opts.canvasTop).attr('title','Shift-Click to open image only');
        $(window).scrollTop(0);

        if (typeof FlashCanvas != "undefined") {
            FlashCanvas.initElement(canvas[0]);
        }		
            var ctx = canvas[0].getContext('2d');
        
        var imageLoaded;		
        var imagesLoaded = 0;	
        var images = [];	
        var el = $(this);

        
        function log(a){
            if (opts.logging){
                console.log(a);
            }
        }     
        
        findImages(el);
            
        $.each(document.images,function(i,e){
            preloadImage($(e).attr('src'));
        });
      
        
        if (images.length == 0){
            start();
        }


        function start(){
       
            if (images.length == 0 || imagesLoaded==images.length/2){    
                log('Started parsing');
                newElement2(el);		
                parseElement(el);                         
            }            
        }
        
        function preloadImage(src){
            
            if (images.indexOf(src)==-1){
                images.push(src);
                   
                var img = new Image();   
                $(img).load(function(){
                    imagesLoaded++;
                    start();
                    
                });	
                img.onerror = function(){
                    images.splice(images.indexOf(img.src),2);
                    imagesLoaded++;
                    start();                           
                }
                img.src = src; 
                images.push(img);
                  
            }     
          
        }
        
        function findImages(el,parseStart){
              
            $(el).contents().each(function(i,element){              
                findImages(element,parseStart);
            });       
            
            
            if (el.nodeType==1 || typeof el.nodeType == "undefined"){
                var background_image = $(el).css('background-image');
                if (background_image && background_image != "1" && background_image != "none"){
                    var src = backgroundImageUrl(background_image);
                     
                    preloadImage(src,parseStart);
                    
                }
            }
        }            
        
        function parsing(el){

            		
            newElement2(el);
							
            if (el.nodeName!="IFRAME"){
                if ($(el).contents().length == 1){
                    //console.log($(el).contents());
					
                    if ($(el).contents()[0].nodeType==1){
                        parsing($(el).contents()[0]);
                    }else{                   
                        newText2(el,$(el).contents()[0]);
                    }
                }else{
						
                    $(el).contents().each(function(cid,cel){
					
                        if (cel.nodeType==1){
                            // element
                            parsing(cel);								
                        }else if (cel.nodeType==3){                   
                            newText2(el,cel);								
                        }              
						
                    });
                }
            }	
        }
			
			
        function parseElement(element){
            $(element).children().each(function(index,el){
		
                parsing(el);
										
            });
            finish();
        }
		
        function backgroundImageUrl(src){
            if (src.substr(0,5)=='url("'){
                src = src.substr(5);
                src = src.substr(0,src.length-2);                 
            }else{
                src = src.substr(4);
                src = src.substr(0,src.length-1);  
            }
            return src;            
        }        
        function loadImage(src){
           
            
	
            var imgIndex = images.indexOf(src);
            if (imgIndex!=-1){
                return images[imgIndex+1];
            }else{
                return false;
            }
           
            
				
        }
			
        function drawImage(img,x,y){

            ctx.drawImage(img,x,y);
        }


        function newElement2(el){
				
		
            if (el.getBoundingClientRect){	
                var bounds = el.getBoundingClientRect();		
	
                var x = bounds.left;
                var y = bounds.top;
                var w = bounds.width;
                var h = bounds.height;
            }else{
                var p = $(el).offset();
                var x = p.left +  parseInt($(el).css('border-left-width'));
                var y =  p.top +   parseInt($(el).css('border-top-width'));
                var w = $(el).innerWidth();
                var h = $(el).innerHeight();
            }
            
            var bgcolor = $(el).css('background-color');
            var background_image = $(el).css('background-image');
            var background_repeat = $(el).css('background-repeat');
            var background_position = $(el).css('background-position');
			
            var border_top_width =  parseInt($(el).css('border-top-width'));
            var border_top_color = $(el).css('border-top-color');
            var border_right_width =  parseInt($(el).css('border-right-width'));
            var border_right_color = $(el).css('border-right-color');
            var border_bottom_width =  parseInt($(el).css('border-bottom-width'));
            var border_bottom_color = $(el).css('border-bottom-color');
            var border_left_width =  parseInt($(el).css('border-left-width'));
            var border_left_color = $(el).css('border-left-color');
            
            
            

            if (border_top_width>0){
				
                newRect(x-border_left_width,
                    y-border_top_width,
                    w+border_left_width+border_right_width,
                    border_top_width,
                    border_top_color);					
            }


            if (border_left_width>0){		

                newRect(x-border_left_width,
                    y,
                    border_left_width,
                    h,
                    border_left_color);					
            }

            if (border_right_width>0){	

                newRect(x+w,
                    y,
                    border_right_width,
                    h,
                    border_right_color);					
            }
				
				
            if (border_bottom_width>0){	

                newRect(x-border_left_width,
                    y+h,
                    w+border_left_width+border_right_width,
                    border_bottom_width,
                    border_bottom_color);					
            }
				
				
            newRect(x,y,w,h,bgcolor);
           
            
            if (background_image && background_image != "1" && background_image != "none"){
                background_image = backgroundImageUrl(background_image);
                image = loadImage(background_image);
					
                var bgposition = background_position.split(" ");
                var background_position_left = parseInt(bgposition[0]);
                var background_position_top = parseInt(bgposition[1]);
				


                if (image){
                    switch(background_repeat){
					
                        case "repeat-x":
                        
                            for(bgx=(x+background_position_left);bgx<=w+x;){   
                                if ( Math.floor(bgx+image.width)>w+x){
                                    ctx.drawImage(image,bgx,(y+background_position_top),(w+x)-bgx,Math.min(image.height,h));
                                }else{
                                    ctx.drawImage(image,bgx,(y+background_position_top));
                                }
                                
                                bgx = Math.floor(bgx+image.width); 

                                
                            } 
                                              
                            break;
                            
                        case "repeat-y":
                        
                            for(bgy=(y+background_position_top);bgy<=h+y;){   
                                if ( Math.floor(bgy+image.height)>h+y){
                                    ctx.drawImage(image,(x+background_position_left),bgy,Math.min(image.width,w),(h+y)-bgy);
                                }else{
                                    ctx.drawImage(image,(x+background_position_left),bgy);
                                }
                                
                                bgy = Math.floor(bgy+image.height); 

                                
                            } 
                                              
                            break;
                            
                        case "no-repeat":
                            drawImage(image,(x+background_position_left),(y+background_position_top));
							
                            break;
					
                    }	
                }else{
                    
                    log("Error loading background:" + background_image);
                //console.log(images);
                }
					
            }
            if (el.nodeName=="IMG"){
                image = loadImage($(el).attr('src'));
                if (image){
                    drawImage(image,x,y);
                }else{
                    log("Error loading <img>:" + $(el).attr('src'));
                }
            }
            
			
				
        }

        
			
        function newRect(x,y,w,h,bgcolor){
            if (bgcolor!="transparent"){
                ctx.fillStyle = bgcolor;
                ctx.fillRect (x, y, w, h);
            }
        }
			
 		
     
        
        
        function printText2(currentText,x,y){
            if ($.trim(currentText).length>0){					
                ctx.fillText(currentText,x,y);
            }           
        }
        
        function newText2(el,textNode){
				
                               
            //var textLeft;
            // var  textTop;
            var align =$(el).css('text-align');
            var family = $(el).css('font-family');
            var size = $(el).css('font-size');
            var color = $(el).css('color');
            var text = $.trim($(el).text());
            var bold = $(el).css('font-weight');
            var font_style = $(el).css('font-style');
            var lineheight =  parseInt($(el).css('line-height'));
            var maxwidth =  $(el).width();
            var text_transform = $(el).css('text-transform');
            var text_decoration = $(el).css('text-decoration');
            var text = textNode.nodeValue;                   
             
				
            switch(text_transform){
                case "lowercase":
                    textNode.nodeValue = text.toLowerCase();
                    break;
					
                case "capitalize":
                    textNode.nodeValue = text.replace( /(^|\s)([a-z])/g , function(m,p1,p2){
                        return p1+p2.toUpperCase();
                    } );
                    break;
					
                case "uppercase":
                    textNode.nodeValue = text.toUpperCase();
                    break;
				
            }
			
			
            //text = $.trim(text);
            if (text.length>0){
                switch(bold){
                    case "401":
                        bold = "bold";
                        break;
                }
            

                ctx.font = bold+" "+font_style+" "+size+" "+family;
                ctx.fillStyle = color;
                ctx.textBaseline = "bottom";
					
               		
                
                var oldTextNode = textNode;
                for(var c=0;c<text.length;c++){
                    var newTextNode = oldTextNode.splitText(1);

                    /*
                    if (document.createRange){
                        var range = document.createRange();
                        range.selectNode(oldTextNode);
                    }else{
                        // todo add IE support
                        var range = document.body.createTextRange();
                    }
                    if (range.getBoundingClientRect()){
                        var bounds = range.getBoundingClientRect();
                    }else{
                        var bounds = {};
                    }*/
                    var parent = oldTextNode.parentNode;
                    var wrapElement = document.createElement('wrapper');
                    var backupText = oldTextNode.cloneNode(true);
                    wrapElement.appendChild(oldTextNode.cloneNode(true));
                    parent.replaceChild(wrapElement,oldTextNode);
                    
                    if (wrapElement.getBoundingClientRect){
                        var bounds = wrapElement.getBoundingClientRect();
                    }else{
                        var bounds = {};
                    }
                    
                    parent.replaceChild(backupText,wrapElement);
                                  
                    printText2(oldTextNode.nodeValue,bounds.left,bounds.bottom);
                    
                    if (text_decoration=="underline"){		                    								
                        newRect(bounds.left,bounds.bottom,bounds.width,1,color);
                    }	
                    oldTextNode = newTextNode;
                  
                  
                  
                }
         
					
            }
			
        }
        
        function finish(){
            log("Finished");
            //canvas.appendTo($('body'));
            opts.ready(canvas);
            
            
        }
        
			
        var a = $("p").position();
        log("Reached the end");
        
    
			
			
    }
		
			
})(jQuery);
			
			
		
	