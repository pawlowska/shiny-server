var ss = document.createElement("link");
ss.type = "text/css";
ss.rel = "stylesheet";
ss.href = window.self === window.top ? "FullSite.css" : "InFrame.css"
document.getElementsByTagName("head")[0].appendChild(ss);