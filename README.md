# TIPE

C'est mon TIPE pour l'année 2017 - 2018 sur le morpion.

Cette intelligence artificialle peut jouer avec le manager suivant:

http://petr.lastovicka.sweb.cz/games.html#piskvorky - Piskvork 8.0.2 - piskvork.zip

Si vous voulez changer le code, vous pouvez:

1. Install Windows (or Wine for Linux, originally the project was created and tested on Ubuntu 16.04 using Wine)
2. Install Python (the code and also following instructions are for version 2.7).
3. Install pywin32 Python package: C:\Python27\Scripts\pip.exe install pypiwin32 (if not present "by default")
4. Install PyInstaller: C:\Python27\Scripts\pip.exe install pyinstaller

Le document .py peut être utilisé avec les commandes suivantes:

cd C:\path\where\the\files\were\saved

C:\Python27\Scripts\pyinstaller.exe pbrain-pengvX.Y.py pisqpipe.py --name pbrain-pyrandom.exe --onefile


Pour plus d'information, vous pouvez aller vers les sites:
1. https://github.com/stranskyjan/pbrain-pyrandom
2. http://gomocup.org/category/news/
