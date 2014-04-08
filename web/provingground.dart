import 'dart:html';
import 'package:angular/angular.dart';
import 'dart:js';

// Temporary, please follow https://github.com/angular/angular.dart/issues/476
@MirrorsUsed(override: '*')
import 'dart:mirrors';


JsObject hub = context['MathJax']['Hub'];



void main() {
  
  ngBootstrap();
  
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
  var mathjax = hub.callMethod("getAllJax" ,["texthis"])[0];
  mathjax.callMethod("Text",['x^2+1']);
  querySelector("#sample_text_id").text = buffer.toString();
}
