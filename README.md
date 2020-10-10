## shiny로 인터렉티브 웹 만들기

**shiny**로 **광양시** 관련 interactive 웹을 만들어보는 프로젝트입니다.

필요한 패키지는 다음과 갔습니다.

```R
install.packages("shiny")
install.packages("shinythemes")
install.packages("leaflet")
install.packages("sf")
install.packages("tidyverse")
```

웹앱을 만들어주는 ```shiny```관련 패키지와 **지도**를 나타내주는 ```leaflet```패키지와 공간정보를 처리하는 ```sf(simple features)```패키지를 설치해야 합니다.

패키지를 모두 설치했다면 프로젝트 열고 실행하시면 됩니다.

1. 콘솔에서 실행하는 방법

```R
> library(shiny)
> runApp("app.R")
```

2. Rscript에서 하나씩 실행도 됩니다.