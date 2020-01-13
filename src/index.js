import "../css/bulma.css";
import "../css/bulma-extensions.min.css";
import "../css/style.css";
import { Elm } from "./Main.elm";
import * as Tone from "./Tone";
import pell from "./pell.min";
import * as serviceWorker from "./serviceWorker";

const tokenName = "grease-token";

var storedState = localStorage.getItem(tokenName);
var app = Elm.Main.init({
  flags: storedState,
  node: document.getElementById("root")
});
var synth = new Tone.Synth().toMaster();

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
    element.scrollIntoView({ behavior: "smooth" });
  }
});
app.ports.playPitch.subscribe(function(halfStepsFromA) {
  synth.triggerAttackRelease(Tone.Midi("A4").transpose(halfStepsFromA), "1n");
});
app.ports.deployEditor.subscribe(function(editorInit) {
  setTimeout(() => {
    const editorElement = document.getElementById(editorInit.elementId);
    if (!editorElement) {
      return;
    }
    const editor = pell.init({
      element: editorElement,
      onChange: html => {
        const element = document.getElementById(editorInit.elementId);
        if (element) {
          element.dispatchEvent(new InputEvent("oninput", { data: html }));
        }
      }
    });
    editor.content.innerHTML = editorInit.content;
  }, 20);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
