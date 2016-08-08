// ==UserScript==
// @name         千ノ落涙 plus
// @namespace    http://tsubasa.osak.jp
// @version      1.0
// @description  Improve ICFPC problem details
// @author       keita_k
// @match        http://2016sv.icfpcontest.org/problem/view*
// @grant        none
// ==/UserScript==
var here = window.location.href.split("/");
var id = here[here.length - 1];
var url ="http://tsubasa.osak.jp/vis/show/problem/" + id;
var tag = document.getElementsByClassName('page-header')[0];
var newContent = tag.innerHTML + "<a target=\"_blank\" href=\"" + url + "\">(Vis)</a>";
tag.innerHTML = newContent;