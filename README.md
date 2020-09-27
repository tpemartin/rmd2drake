# rmd2drake
 A package that turn R markdown into a drake plan with tailored make/vis functions

## installation
```
remotes::install_github("tpemartin/rmd2drake")
```

## video demo
[How to use rmd2drake](https://vimeo.com/462045339)

## attentions

使用這套件有幾件事要注意：1. 模版必需開在某個project資料匣裡，所以你要先創一個project, 或用舊project; 2. 模版開啟後要先「存檔」才能按addin，因為addins會依照模版的位置把檔案內容叫進來處理，若沒有存檔，addins會抓不到檔案。（addins有一個get .activieFile，你可以按一下然後看.activeFile 內容的路徑是不是你現在rmd檔的位置.


