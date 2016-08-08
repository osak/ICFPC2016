// ==UserScript==
// @name         逆鱗チーター
// @namespace    http://tsubasa.osak.jp
// @version      1.1
// @description  逆鱗チートモード
// @author       keita_k
// @match        http://tsubasa.osak.jp/gekirin/*
// @grant        none
// ==/UserScript==

//gekirin must have jquery

(function() {
    'use strict';

    var here = window.location.href.split("=");
    var id = here[here.length - 1];
    var gekirin = $('.col-lg-4');

    var cheat = $('<input>').attr({
        type: 'text',
        id: 'cheatCommand',
        placeholder: 'input cheat command'
    });
    

    var runCommand = function(com) {
        if(com == 'a') {
            $('#select-all-facet').click();
        } else if (com[0] == 's') {
            var ids = '#skeleton-button' + com.substring(1);
            $(ids).click();
        } else if (com[0] == 'f') {
            if (com[1] == 'f') {
                var stModEnd = com.substring(2);
                var sme = stModEnd.split("_");
                var st = sme[0] - 0;
                var mod = sme[1] - 0;
                var end = sme[2] - 0;
                console.log(sme);
                if (mod == 0) {
                    console.log("Dangerous mod");
                    return;
                }
                for (var i = st; i <= end; i += mod) {
                    console.log(i);
                    var ids = '#facet-button' + i;
                    $(ids).click();
                }
            } else {
                var idf = '#facet-button' + com.substring(1);
                $(idf).click();
            }
        } else if (com == 'l') {
            var checked = $('input[name="direction"]:checked');
            var unchecked = $('input[name="direction"]:not(:checked)');
            var val = checked.val();
            if (val == -1) {
                //swap
                checked.prop("checked",false);
                unchecked.prop("checked", true);
            }
        } else if (com == 'r') {
            var checked = $('input[name="direction"]:checked');
            var unchecked = $('input[name="direction"]:not(:checked)');
            var val = checked.val();
            if (val == 1) {
                //swap
                checked.prop("checked",false);
                unchecked.prop("checked", true);
            }
        }
    };
    
    var makeCommandButton = function(command) {
        return $('<button/>', {
            text: command,
            click: function () {
                //console.log(this);

                var coms = this.innerHTML.split(" ");
                console.log(coms);
                for (var i = 0; i < coms.length; i++) {
                    if (coms[i].length === 0) {
                        continue;
                    }
                    runCommand(coms[i]);
                }
            }
        });            
    };
    
    var loadCheats = function() {
        var key = 'cheats_' + id;
        var cheats = localStorage.getItem(key);
        if (cheats !== null) {
            cheats = JSON.parse(cheats);
            var cheatsList = $('<div/>', {
                id: 'cheatsList',
            });
           
            for (var j = 0; j < cheats.length; j++) {
                //console.log(cheats[j]);
                var button = makeCommandButton(cheats[j]);
                //console.log(button);
                cheatsList.append(button);
            }
            //console.log(cheatsList);
            return cheatsList;
        }
        return null;
    };

    var test = $('<button/>', {
        text: 'test',
        id: 'testbutton',
        click: function () {
            var coms = $('#cheatCommand').val();
            var com = coms.split(" ");
            for (var i = 0; i < com.length; i++) {
                if (com[i].length === 0) {
                    continue;
                }
                runCommand(com[i]);
            }
        }
    });
    var save = $('<button/>', {
        text: 'save',
        click: function () {
            var coms = $('#cheatCommand').val();
            
            var key = 'cheats_' + id;
            
            var cheats = localStorage.getItem(key);
            console.log(cheats);
            if (cheats === null) {
                cheats = [];
            } else {
                cheats = JSON.parse(cheats);
            }
            console.log(cheats);
            cheats.push(coms);
            $("#cheatsNowArea").append(makeCommandButton(coms));
            localStorage.setItem(key, JSON.stringify(cheats));
        }
    });

    gekirin.after(cheat);
    cheat.after(test);
    test.after(save);
    var cheatsNowArea = $('<div>').attr({
        id: 'cheatsNowArea'
    });
        save.after(cheatsNowArea);
    var cheatsList = loadCheats();
    if (cheatsList !== null) {
        save.after(cheatsList);
    }
    

})();



