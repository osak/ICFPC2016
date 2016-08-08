// ==UserScript==
// @name         千ノ落涙
// @namespace    http://tsubasa.osak.jp
// @version      1.0
// @description  Improve ICFPC problem list
// @author       osa_k
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