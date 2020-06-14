var _ = undefined;

function abort(msg,hard=true){
  var might = hard? 'will' : 'might';
  var dv = document.createElement('div');
  var doc = document.children[0].children[1];
  dv.class="container row";
  dv.innerHTML = msg+' Please inform Sebastiaan about this (send him a screenshot). Several things '+might+' not work right now. After Sebastiaan asks you to try again, refresh this page with cmd-shift-R or ctrl-shift-R.<br />';
  if(hard){
    doc.appendChild(dv);
    exit(1);
  }else{
    doc.insertBefore(dv,doc.childNodes[0]);
  }
}

// Helper to display a field (first argument: q)
// Does two things (depending on q):
// 1. Create a value-object with its relevent retrieve function, and pass it to pushHandler to be added to an array.
//    You typically want to pass .push of some array for 'pushHandler'.
// 2. Generate the appropriate DOM node and call appendHandler with it (provided there's something to see).
//    You typically want to pass .appendChild of some DOM element for 'appendHandler'.
// Results to calls of both handlers are ignored and default to ignoring functions.
function handleField(q, pushHandler = ()=>_, appendHandler = ()=>_)
{
  switch(q.tag){
    case null: console.log(q); abort('Malformed field: q.tag = null'); appendHandler(document.createTextNode("[[- Null encountered in q.tag -]]"));
    break; case "FText": 
          appendHandler(document.createTextNode(q.contents));
    break; case "FFieldMath":
          var elmt = document.createElement('div');
          elmt.className="u-full-width";
          // non-editable: MQ.StaticMath
          var mf = MQ.MathField(elmt,{handlers:{enter:function(){submitAction();}}} );
          pushHandler({name:q.contents, q:q, getVal:function(){return mf.latex();}});
          appendHandler(elmt);
    break; case "FValueS":
          pushHandler({name:q.fvName, q:q, getVal:function(){return q.fvValS;}});
    break; case "FValue":
          pushHandler({name:q.fvName, q:q, getVal:function(){return q.fvVal;}});
    break; default:
          appendHandler(document.createTextNode('[[ '+q.tag+' ]]'));
          console.log(q);
          abort('Unknown field tag: '+q.tag, false);
  }
}

var MQ;
var ses;
var serverCGI = "/~sjc/cs30/cs30.cgi";
var currentlyCallingAPI = false;
var disconnects = 0;
var serverResponseMsg;
window.onload = function (){
  if(window.top==window.self){ document.body.classList.add("top"); }
  
  if (typeof $ == 'undefined'){
    abort('Could not find jquery.');
  }
  // abort('Trying to cause problems ....',false)
  if(typeof MathQuill === 'undefined'){
    abort('Could not load MathQuill.',true);
  }else{
    MQ = MathQuill.getInterface(2);
  }
  var mathFieldSpan = document.getElementById('math-field');
  $(".math-field").each(function(i,mf){
      var v= MQ.MathField(mf,{
                spaceBehavesLikeTab: false, // configurable
                handlers: {
                  edit: function() {}
                }
                });
      mf.v=v;
    }
  );
  $("input.submit").each(function(i,ip){
    var jip = $(ip);
    jip.on("click",function(e){
      var data = [];
      // gather values in an array:
      jip.closest("form").find(".math-field").each(function(i,mf){
        data.push({n:mf.id,v:mf.v.latex()});
      });
      console.log(data);
      console.log(e);
      return false;
    });
  });
  serverResponseMsg = $('#serverCGI');
  function paragraph(txt){
    var p = document.createElement('p');
    p.appendChild(document.createTextNode(txt));
    return p;
  }
  function span(txt){
    var p = document.createElement('span');
    p.appendChild(document.createTextNode(txt));
    return p;
  }
  function pre(txt){
    var p = document.createElement('pre');
    p.appendChild(document.createTextNode(txt));
    return p;
  }
  function communicate(data,handler=processPageUpdate){
    data.r = 0;
    data.ses = ses;
    processError = function (obj,statusStr){
      var errMsg = document.createElement('div');
      errMsg.className="row error";
      var h3 = document.createElement('h3');
      h3.appendChild(document.createTextNode('Error in communication between JavaScript running on your machine and code on the server'));
      errMsg.appendChild(h3);
      var reloadLink = document.createElement('a');
      reloadLink.appendChild(document.createTextNode("Click here to try again"));
      reloadLink.className = "button";
      reloadLink.onclick = function()
        {
            $.ajax(request);
            serverResponseMsg.each(function(i,e){
              e.removeChild(errMsg);
            });
        };
      data.r++;
      if(obj.readyState==0 && obj.status==0){
        disconnects++;
        errMsg.appendChild(paragraph('This may be a connection error, check your internet connection'));
        errMsg.appendChild(reloadLink);
        errMsg.appendChild(paragraph('Alternatively, take a screenshot and email Sebastiaan'));
      }else{
        console.log(obj);
        var err = document.createElement('p');
        err.innerHTML = "The error was: <em>"+statusStr+"</em>";
        errMsg.appendChild(err);
        errMsg.appendChild(paragraph('Please take a screenshot and email Sebastiaan'));
        errMsg.appendChild(paragraph('Some debug output for Sebastiaan:'));
        errMsg.appendChild(pre(obj.responseText));
        errMsg.appendChild(paragraph('After letting Sebastiaan know about this issue (probably his fault), you may want to try the same action again by clicking below:'));
        errMsg.appendChild(reloadLink);
      }
      errMsg.appendChild(document.createElement('hr'));
      serverResponseMsg.html(errMsg);
    };
    var request = {
                dataType: "json", // json
                method : "POST",
                url: serverCGI,
                data: data,
                success: handler,
                jsonp: false,
                error: processError
              };
    $.ajax(request);
  }
  function card(title,content,level=3){
    var div = document.createElement('div');
    if (level < 5){ /* move to half cards */
      div.className = "card columns six";
    }
    div.className = "card";
    var h = document.createElement('h'+level);
    h.appendChild(document.createTextNode(title));
    div.appendChild(h);
    if(level < 3){
      var hr = document.createElement('hr');
      div.appendChild(hr);
    }
    $(div).append(content);
    return div;
  }
  function processPageUpdate(data,statusStr,jqXHR){
    serverResponseMsg.html(''); // clear any of the server's error messages still on the screen
    if (data.rEcho != null){
      history.pushState(data.rEcho, data.rEcho, '?'+data.rEcho);
      window.onpopstate = function(event)
        {
          communicate({cAct:'page',
                  s:event.state
                  });
        };
    }
    if (data.rSes != null) ses = data.rSes;
    if (data.rLogin != null) {
      $('#login').html(data.rLogin);
      $('#login2').html(data.rLogin);
    }else{
      $('#login').html('Not logged in');
      $('#login2').html('Not logged in');
    }
    var cards = $('#cards');
    var exrs = data.rExercises;

    var pages = data.rPages;
    if(pages.length > 0){
      $('#header').show();
      $('#login2').hide();
      var el = $('#nav-exercise-list');
      el.empty();
      for(var i=0;i<pages.length;i++){
        (function(page){
          var a = document.createElement('a');
          a.className = "button u-full-width";
          // $(a).addClass("button u-full-width");
          a.appendChild(document.createTextNode(page.pName));
          (function () { 
            var pid = page.pId;
            // note: without using jQuery's "on", jQuery's "clone" won't copy the event.
            $(a).on('click', function()
              { communicate({ s:pid, echo:pid }); });
          })();
          el.append(a);
        })(pages[i]);
      }
      if (exrs.length == 0) {
        cards.empty();
        // repeat the pages as a main selection thingy if there are no exercises
        cards.append(card("Exercises",el.clone(true).removeClass('dropdown-content'),2));
      }
    }

    if (exrs.length > 0) {
      cards.empty();
      for(var i=0;i<exrs.length;i++){
        // Print each exercise as a card
        (function(exr){
          var submitAction;
          var qs = exr.eQuestion;
          var hs = exr.eHidden;
          var valuesH = [];
          for(var j=0;j<hs.length;j++){
            handleField(hs[j], valuesH);
          }
          var values = [];
          var d = document.createElement('div');
          d.className="row";
          for(var j=0;j<qs.length;j++){
            handleField(qs[j], values, d.appendChild);
          }
          var buttonRow = document.createElement('div');
          buttonRow.className="columns row";
          var acs = exr.eActions;
          for(var j=0;j<acs.length;j++){
            (function(ac){
              var f = function(){
                var data = {cAction:ac};
                var vdata = {};
                for(var j=0;j<values.length;j++){
                  vdata[values[j].name] = values[j].getVal();
                }
                var hidden = {};
                for(var j=0;j<valuesH.length;j++){
                  data[valuesH[j].name] = valuesH[j].getVal();
                }
                data.cValue = vdata;
                communicate({ex:JSON.stringify(data)});
              };
              elmt = document.createElement('button');
              elmt.type = "button";
              elmt.className="button two u-pull-right";
              elmt.appendChild(document.createTextNode(ac.tag));
              elmt.onclick = f;
              submitAction = f;
              buttonRow.appendChild(elmt);
            })(acs[j]);
          }
          d.appendChild(buttonRow);
          // elmt = document.createElement('div');
          // elmt.className = "row";
          // d.appendChild(document)
          var theCard = card(exr.eTopic, d,exrs.length>2?4:3);
          cards.append(theCard);
        })(exrs[i]);
      }
    }

    var splash = data.rSplash;
    if (splash!=null) {
      alert('Splash! '+splash);
    }
  }

  if(postData=='')
    communicate({cAct:'page', // 'page' loads top header
              c:document.cookie,
              s:window.location.search});
  else
    communicate({cAct:'login', 
              c:document.cookie,
              s:window.location.search,
              a:postData,
              h:signature // hash
              });
};