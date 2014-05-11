import 'dart:html';
//import 'package:angular/angular_dynamic.dart';
import 'package:angular/angular.dart';
import 'package:angular/application_factory.dart';
import 'dart:convert';

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
    type(ACDstbnController);
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
