import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const tokenName = "grease-token";

var storedState = localStorage.getItem(tokenName);
var startingState = storedState ? JSON.parse(storedState) : null;
var app = Elm.Main.init({
  flags: startingState,
  node: document.getElementById("root"),
});

app.ports.setToken.subscribe(function(token) {
  if (token) {
    localStorage.setItem(tokenName, token);
  } else {
    localStorage.removeItem(tokenName);
  }
});
app.ports.alert.subscribe(function(alertMessage) {
  alert(alertMessage);
});
app.ports.scrollToElement.subscribe(function(elementId) {
  const element = document.getElementById(elementId);
  if (element) {
    element.scrollIntoView();
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
