/* 
 * @author Niklas von Hertzen <niklas at hertzen.com>
 * @created 13.7.2011 
 * @website http://hertzen.com
 */


(function( $ ){
    $.fn.feedback = function(options) {
  
        var self = this;
            
    
        
        var reset = {
            'margin':0              
        },
        createDiv,
        createDivLeft,
        createDivTop,
        overlay,
        feedbackDiv;
        
         var textMessage = $('<textarea />');
        var preview = $('<button />').text('Preview').click(function(){
            
            
        var overlayChildren = overlay.children().clone();
            // var w = window.open();
            overlay.remove();
            
            
            
            feedbackDiv.remove();
            $('#feedback').hide();
            var w = window.open();
            var canvasDom=w.document;
            canvasDom.write('Loading... Please wait');
           // canvasDom.title = "Send feedback";
           
            window.setTimeout(function(){
                var canvas = $('body').html2canvas({
                    ready:function(canvas){
                      
                     
                        var canvasElement = canvas[0];
                        var ctx = canvasElement.getContext('2d');
                        ctx.strokeStyle = "rgb(255,0,0)";
                        ctx.lineWidth = 4;


                         $.each(overlayChildren,function(i,e){
                             ctx.moveTo(0,0);

                             var left = parseInt($(e).css('left'));
                             var top = parseInt($(e).css('top'));
                             var width = $(e).width();
                             var height = $(e).height();
                             
                             ctx.strokeRect(left,top,width,height);
                             
                             
                         });
                       


 


                        
                        var a = canvasElement.toDataURL();
                        canvasDom.body.innerHTML = "";
                    
                        canvasDom.write("<h1>Send Feedback</h1>"+"<img src='"+a+"' style='border:1px solid black; width:400px;' />");  
                        canvasDom.write('<br /><br />Message:<br /><textarea style="width:400px;height:200px;">'+textMessage.val()+'</textarea>');
                        canvasDom.write('<br /><button>Submit (disabled in the example)</button>');
                        
                        $('#feedback').show();
                    }
                  
                });
            
            },1000);
             
        /*
               
                    var a = canvas[0].toDataURL();
                    // window.location.assign(a);
               
                    var w = window.open();
                    var canvasDom=w.document;
                    canvasDom.write("<img src='"+a+"'></img>");
                   */
                   
                
                
          
            
            
        }); 
        
       
               
        feedbackDiv =  $('<div />')
        .css(reset)
        .css({
            'position':'fixed',
            'right':0,
            'bottom':0,
            'background':'#fff',
            'z-index':999
                
        })
        .addClass('feedback-form')
        .append(
            $('<h2 />').text('Send feedback')
        
            )
        .append(
            $('<label />').text('Describe your problem:')
    
            )
        .append(textMessage)
        .append(preview)
        .appendTo('body');
        
        overlay = $('<div />')
        .css(reset)
        .css({
            'background-color':'#000',
            'opacity':0.5,
            'position':'absolute',
            'top':0,
            'left':0,
            'width':$(document).width(),
            'height':$(document).height(),
            'margin':0
            
        })      
        .appendTo('body').mousedown(function(e){
            
            createDiv = $('<div />')
            .css(reset)
            .css({
                //    'border':'3px solid black',
                'position':'absolute',
                'left':e.pageX,
                'top':e.pageY,
                'opacity':1,
                'background':'#fff',
                'cursor':'pointer'
            }).appendTo(overlay);
            
            
            createDivLeft  = e.pageX;
            createDivTop = e.pageY;
            
            overlay.bind('mousemove',function(e){      
              
                createDiv.width(Math.abs(e.pageX-createDivLeft));
                createDiv.height(Math.abs(e.pageY-createDivTop));
                   
                if (e.pageX<createDivLeft){
                    createDiv.css('left',e.pageX);
                }               
                
                if (e.pageY<createDivTop){
                    createDiv.css('top',e.pageY);
                }  
                
            });
            
        }).mouseup(function(e){
            
            var whiteDiv = createDiv;
            var deleteButton;
            var onDelete = false;
            var onWhiteDiv = false;
            
            
            createDiv.click(function(){
                $(this).remove(); 
            });
            
            /*
            createDiv.hover(function(){
                onWhiteDiv = true;
                deleteButton = $('<img />')
                .attr('src','images/Delete.png')
                .css(reset)
                .css({
                    'position':'absolute',
                    'left':parseInt(whiteDiv.css('left'))+whiteDiv.width()-20,
                    'top':parseInt(whiteDiv.css('top'))-10,
                    'cursor':'pointer',
                    'margin':3,
                    'z-index':999
                })
                .hover(function(){
                    onDelete = true;
                },function(){
                    onDelete = false;
                    window.setTimeout(function(){
                        if (!onWhiteDiv){
                            deleteButton.remove();
                        }
                    },0);
                })
                .appendTo('body')
                .click(function(){
                    whiteDiv.remove(); 
                    $(this).remove();
                });
                
            },function(){
                onWhiteDiv = false;
                window.setTimeout(function(){
                    if (!onDelete){
                        deleteButton.remove();
                    }
                },0);
            });
            */
            
            overlay.unbind('mousemove');
           
        });
        
        
        
        
  
    };
})( jQuery );