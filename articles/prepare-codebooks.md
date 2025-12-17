# Prepare Codebooks

``` r
library(eatPrepTBA)
#> ℹ eatPrepTBA v0.9.8.9000
```

## Login Procedure

``` r
login <- login_studio(keyring = TRUE, app_version = "12.3.1")
workspace <- access_workspace(login, ws_id = c(908))
```

If you want to retrieve all codes:

``` r
cb_all <- prepare_codebook(workspace = workspace)
```

``` r
cb_all
#> # A tibble: 206 × 7
#>    unit_key unit_label             variable_id variable_label code_id code_label
#>    <chr>    <chr>                  <chr>       <chr>          <chr>   <chr>     
#>  1 DK_HS01  Demo-Aufgabe Kodierung 01          ""             1       RICHTIG   
#>  2 DK_HS01  Demo-Aufgabe Kodierung 01          ""             INTEND… ABSICHTLI…
#>  3 DK_HS01  Demo-Aufgabe Kodierung 01x         ""             1       RICHTIG   
#>  4 DK_HS01  Demo-Aufgabe Kodierung 01x         ""             0       FALSCH    
#>  5 DK_HS01  Demo-Aufgabe Kodierung 02          ""             1       RICHTIG   
#>  6 DK_HS01  Demo-Aufgabe Kodierung 02          ""             0       FALSCH    
#>  7 DK_HS01  Demo-Aufgabe Kodierung 03a         ""             1       RICHTIG   
#>  8 DK_HS01  Demo-Aufgabe Kodierung 03a         ""             0       FALSCH    
#>  9 DK_HS01  Demo-Aufgabe Kodierung 03b         ""             1       RICHTIG   
#> 10 DK_HS01  Demo-Aufgabe Kodierung 03b         ""             0       FALSCH    
#> # ℹ 196 more rows
#> # ℹ 1 more variable: code_description <chr>
```

## Only Manual Codes

If you want to retrieve only codes:

``` r
cb_manual <- prepare_codebook(workspace = workspace, 
                              manual = TRUE)
```

``` r
cb_manual
#> # A tibble: 206 × 7
#>    unit_key unit_label             variable_id variable_label code_id code_label
#>    <chr>    <chr>                  <chr>       <chr>          <chr>   <chr>     
#>  1 DK_HS01  Demo-Aufgabe Kodierung 01          ""             1       RICHTIG   
#>  2 DK_HS01  Demo-Aufgabe Kodierung 01          ""             INTEND… ABSICHTLI…
#>  3 DK_HS01  Demo-Aufgabe Kodierung 01x         ""             1       RICHTIG   
#>  4 DK_HS01  Demo-Aufgabe Kodierung 01x         ""             0       FALSCH    
#>  5 DK_HS01  Demo-Aufgabe Kodierung 02          ""             1       RICHTIG   
#>  6 DK_HS01  Demo-Aufgabe Kodierung 02          ""             0       FALSCH    
#>  7 DK_HS01  Demo-Aufgabe Kodierung 03a         ""             1       RICHTIG   
#>  8 DK_HS01  Demo-Aufgabe Kodierung 03a         ""             0       FALSCH    
#>  9 DK_HS01  Demo-Aufgabe Kodierung 03b         ""             1       RICHTIG   
#> 10 DK_HS01  Demo-Aufgabe Kodierung 03b         ""             0       FALSCH    
#> # ℹ 196 more rows
#> # ℹ 1 more variable: code_description <chr>
```
