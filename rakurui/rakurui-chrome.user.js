// ==UserScript==
// @name         New Userscript
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  try to take over the world!
// @author       You
// @match        http://2016sv.icfpcontest.org/problem/list*
// @grant        none
// ==/UserScript==
Array.prototype.forEach.call(document.querySelectorAll('table.table tr'), function(tr) {
    var tdId = tr.firstElementChild;
    var id = parseInt(tdId.textContent);
    var newTd = document.createElement('td');
    var url = "http://tsubasa.osak.jp/vis/show/problem/" + id;
    newTd.innerHTML = "<a target=\"_blank\" href=\"" + url + "\">Visualizer</a>";
    newTd.innerHTML += "  <a target=\"_blank\" href=\"" + url + "?force=1\">(Force recompile)</a>";
    tr.appendChild(newTd);
});
