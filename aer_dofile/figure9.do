graph combine "$output/panel_a" "$output/panel_b", col(1) row(2) imargin(tiny) xsize(8.5) ysize(4.5) graphregion(fcolor(white) color(white) icolor(white) margin(tiny))
graph display, xsize(7.5) ysize(10)
graph export "$output/figure9.wmf", replace

erase "$output/panel_a.gph"
erase "$output/panel_b.gph"

