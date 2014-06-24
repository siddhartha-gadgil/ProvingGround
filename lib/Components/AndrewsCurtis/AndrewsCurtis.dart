library provingground.AndrewsCurtis;

import 'dart:html';
//import 'package:angular/angular_dynamic.dart';
import 'package:angular/angular.dart';
import 'dart:convert';

class WtdPres{
  double prob; 
  List<List<int>> wrds;
  int rank;
  
  List gens () => new List.generate(rank, (int i) => unilet(i+1));
  
  List<String> relsuni () => wrds.map(uniword);
  
  WtdPres(this.prob, this.wrds);
  
  WtdPres.fromJson(String js){
    prob = JSON.decode(js)["prob"];
    wrds = JSON.decode(js)["words"];
    rank = JSON.decode(js)["rank"];
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



@Component(
    selector:"andrews-curtis",
    templateUrl: 'packages/ProvingGround/Components/AndrewsCurtis/AndrewsCurtis.html',
    cssUrl: 'packages/ProvingGround/Components/AndrewsCurtis.css',
    publishAs : 'ac')
class AndrewsCurtisComponent{

  EventSource dstbnsrc;

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
  
  AndrewsCurtisComponent(){
    _init;
  }
}