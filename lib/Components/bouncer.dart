import 'dart:html';
//import 'package:angular/angular_dynamic.dart';
import 'package:angular/angular.dart';


@Component(
    selector:"bouncecomp",
    templateUrl: 'packages/ProvingGround/Components/bouncer.html',
    cssUrl: 'packages/ProvingGround/Components/bouncer.css',
    publishAs : 'bnc')
class BounceComponent{
  EventSource bouncesrc;
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