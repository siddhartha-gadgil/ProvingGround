import 'dart:html';
import 'package:angular/angular.dart';
import 'package:js/js.dart';

// Temporary, please follow https://github.com/angular/angular.dart/issues/476
@MirrorsUsed(override: '*')
import 'dart:mirrors';


var hub = context['MathJax']['Hub'];

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
  hub;
//  hub.callMethod("Typeset", []);
  querySelector("#sample_text_id").text = buffer.toString();
}
