library provingground;

import 'dart:html';
//import 'package:angular/angular_dynamic.dart';
import 'package:angular/angular.dart';
import 'package:angular/application_factory.dart';
import 'dart:convert';
import 'package:ProvingGround/Components/bouncer.dart';
import 'package:ProvingGround/Components/AndrewsCurtis/AndrewsCurtis.dart';


// Temporary, please follow https://github.com/angular/angular.dart/issues/476
@MirrorsUsed(override: '*')
import 'dart:mirrors';


// JsObject hub = context['MathJax']['Hub'];

class WtdPres{
  double prob;
  List<List<int>> wrds;
  
  List<String> relsuni () => wrds.map(uniword);
  
  WtdPres(this.prob, this.wrds);
  
  WtdPres.fromJson(String js){
    prob = JSON.decode(js)["prob"];
    wrds = JSON.decode(js)["words"];
  }
  
}

int a = ASCII.encode("a")[0];

String letter (int k) => ASCII.decode([a + k -1]);

String unilet(int k){
  if (k>0) 
    return letter(k);
  else
    return letter(-k) + '\u{0305}';
}

String uniword (List<int> lets) => lets.map(unilet).fold("", (a, b) => a + b);

EventSource dstbnsrc;

EventSource bouncesrc;


@Controller(
    selector:"[bouncer]",
    publishAs : 'bnc')
class BounceController{
  String value;
  num mult;
  List<String> bounces=["init"];
  List<String> logs =["started"];
  
  void push(){
    logs.add("pushed");
    var map = {"value" : value, 'mult' : mult.toString()};
    HttpRequest.postFormData("../../bounce", map)
    .then((resp){
      logs.add(resp.responseText);
      })
    .catchError((e){
      logs.add("could not push");
    });
  }
  
  BounceController(){
    bounces.add(";create");
    bouncesrc = new EventSource("../../bouncestream")
          ..onMessage.listen((event){
      bounces.add(event.data);
      logs.add("tick");
        });
  }
}


@Controller (
    selector: "[ACdistribution]",
    publishAs : "dstbn")
class ACDstbnController{
  List <WtdPres> distbn;
  
  void updateDstbn(List<WtdPres> newdstbn){
    distbn = newdstbn;
  }
  
  void _init() {
  dstbnsrc = new EventSource("../dstbns")
        ..onMessage.listen((event){
    List wps = JSON.decode(event.data);
    List <WtdPres> newdistbn = wps.map((wp) => new WtdPres.fromJson(wp));
    updateDstbn(newdistbn);
      });
  }
  
  ACDstbnController(){
    _init();
  }
}

class MyAppModule extends Module {
  MyAppModule() {
    bind(BounceController);
    bind(BounceComponent);
    bind(AndrewsCurtisComponent);
  }
}


void main() {
  
  applicationFactory().addModule(new MyAppModule()).run();
  
  querySelector("#sample_text_id")
      ..text = "Click me!"
      ..onClick.listen(reverseText);
}

void reverseText(MouseEvent event) {
  var text = querySelector("#sample_text_id").text;
  var buffer = new StringBuffer();
  for (int i = text.length - 1; i >= 0; i--) {
    buffer.write(text[i]);
  }
//  var mathjax = hub.callMethod("getAllJax" ,["texthis"])[0];
//  mathjax.callMethod("Text",['x^2+1']);
  querySelector("#sample_text_id").text = buffer.toString();
}
