library(plm)
pgr <- plm.data(data, index = c("firm", "year")) 
gr_pool <- plm(LOGOUT ~ LOGLABOR + LOGKAP, data = pgr, 
               model = "pooling")
gr_fe <- plm(LOGOUT ~ LOGLABOR + LOGKAP, data = pgr, 
             model = "within") 
pFtest(gr_fe, gr_pool)
gr_re <- plm(LOGOUT ~ LOGLABOR + LOGKAP, data = pgr, 
             model = "random", random.method = "swar") 
summary(gr_re) 
phtest(gr_re, gr_fe)
phtest(gr_re, gr_fe)