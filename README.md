Turingův stroj
==============
Projekt FLP-log 2016
--------------------

* autor: Vojtěch Dvořáček
* email: xdvora0y@stud.fit.vutbr.cz
* datum: 28.4.2016

Spuštění programu
-----------------

Přiložený Makefile generuje ze zdrojového souboru `flp16-log.pl` spustitelný soubor
`flp16-log`. Ten přijímá jako parametr název vstupního souboru.
Pokud není zadán, očekává vstup na STDIN.

Příklad spuštění: 

* `$./flp16-log examp.ts`
* `$./flp16-log < nts.ts`

Testovací vstupy jsou v adresáři examples.
Délka výpočtu je v řádu vteřin.

Popis implementace
------------------

Program implementuje simulátor NTS. Na základě zadané vstupní pásky a pravidel simuluje výpočet.
Výpočet končí:

1. dosažením koncového stavu 'F'
2. abnormálně - nenajde-li pro daný stav aplikovatelný přechod
3. po nastavitelném počtu kroků pro případ zacyklení - predikát `cycle_limit(N)`

V případě nedeterministické situace simuluje NTS variantu prohlédávání do šířky, kdy postupně vytváří
alternativní historie výpočtu. Pokud některá končí úspěchem je navrácena.

