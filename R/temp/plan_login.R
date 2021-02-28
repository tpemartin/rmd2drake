# make plan -----------------
# no params in the frontmatter

library(htmltools); library(rlang)
library(dplyr); library(webtemplate)

rmd2drake:::extract_activeEditorFilename()
setwd(dirname(.activeFile))

title = "Programming for Data Science"
emajorIconUrl="img/emajor_logo.jpg"

# make plan -----------------
params=readRDS("params_googleLogin.rds")

library(readr)
library(dplyr)
library(htmltools); library(rlang)
library(webtemplate)
library(glue)
rprojroot::is_rstudio_project -> pj
pj$make_fix_file() -> root

jsPath = file.path(root(),"js/onSignIn.js")
htmlPath=file.path(root(),"html/signOut.html")

# plan_login------------
plan_login=drake::drake_plan(
# > plan begins -----------
# >> myChoice--------------
myChoice = {
  menu <- get_menu()
  myChoice <- list(
    menu$materialize(),
    menu$jQuery(),
    menu$googleLogin()
  )
  myChoice
},

# >> loginCard--------------
loginCard = {
  div(
    class="row",
    div(
      class="col s4 offset-s4",
{
  div(
    class = "card blue-grey darken-1",
    {
      div(
        class = "card-content white-text",
        span(
          class = "card-title",
          "資料科學程式設計（一）"
        ),
        div(
          class="divider"
        ),
        div(
          class="section",
          p(
          "請使用Google classroom之gmail登入"
          )
        ),
        div(
          class="section",
          withinbodyHtml
        )
      )
    }
  )
}
    )
  )
},

# >> withinbodyHtml--------------
withinbodyHtml={
  # google login
webtemplate::extract_withinbodyHtml(reviseMyChoice)},

# >> outputWeblayout--------------
outputWeblayout = {
  bodyLayout <- generate_template(
    reviseMyChoice,
    .body=tagList(loginCard) #bodyTaglist
  )
  save_html(bodyLayout, file=file_out("login.html"))
},

# >> insert_navbar--------------
insert_navbar = function(listOfLiContents){
  build_liGroup(listOfLiContents) -> liContent
  htmlVerbose = glue::glue(
  '<nav>
      <div class="nav-wrapper">
        <a href="#" class="brand-logo right">Logo</a>
        <ul id="nav-mobile" class="left hide-on-med-and-down">
    {liContent}
        </ul>
      </div>
    </nav>')
  htmltools::htmlTemplate(text_=htmlVerbose)
},

# >> googleAfterbodyHtmlFix--------------
googleAfterbodyHtmlFix = "<script>
    var user;
    function onSuccess(googleUser) {
       user=googleUser;
      console.log('Logged in as: ' + googleUser.getBasicProfile().getName());
      window.localStorage.setItem(\"googleUserId\",Ca);
      window.localStorage.setItem(\"user\",user)
    }
    function onFailure(error) {
      console.log(error);
    }
    function renderButton() {
      gapi.signin2.render('my-signin2', {
        'scope': 'profile email',
        'width': 240,
        'height': 50,
        'longtitle': true,
        'theme': 'dark',
        'onsuccess': onSuccess,
        'onfailure': onFailure
      });
    }
  </script>

<scriptsrc=\"https://apis.google.com/js/platform.js?onload=renderButton\"asyncdefer></script>",

# >> reviseMyChoice--------------
reviseMyChoice = {
  readr::read_file(file_in("googleAfterbodyHtml.html")) ->
    myChoice[[3]]$afterbodyHtml
  myChoice
}

# > plan ends ------------
)

# plan_googleLogin------------
plan_googleLogin=drake::drake_plan(
# > plan begins -----------
# >> sub1_platformJs--------------
sub1_platformJs = {
  htmltools::htmlDependency(
  name="google api",
  version = "v0",
  src=c(href="https://apis.google.com"),
  script="js/platform.js",
  meta = list(
    name=tags$meta(
      name="google-signin-client-_id",
      content=params$clientId
      )
    )
  ) -> platformJS
  htmltools::tagAppendAttributes(platformJS, "async defer")
},

# >> sub1_signinBtn--------------
sub1_signinBtn = {
  div(
    class="g-signin2",
    `data-onsuccess`="onSignIn"
  )
},

# >> sub1_signInJs--------------
sub1_signInJs = {
  
  tagExpr = rlang::expr(htmltools::includeScript(file_in(!!jsPath)))

  rlang::eval_tidy(tagExpr)
},

# >> sub1_signOutHtml--------------
sub1_signOutHtml = {
  tagExpr = rlang::expr(htmltools::includeHTML(file_in(!!htmlPath)))

  rlang::eval_tidy(tagExpr)
}

# > plan ends ------------
)

