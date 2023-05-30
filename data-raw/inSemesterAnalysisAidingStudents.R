



make.assign.params <- function() {



}

tb <- tibble(n=rep_len(x=5, length.out=5), size=rep_len(x=25, length.out=5), prob=probs)
tb
map2_dfr(tb, .x=n, .y=prob, .f=rbinom)
map2_dfr(tb, .x=tb$n, .y=tb$prob, .f=rbinom)
pmap(tb, .x=tb$n, .y=tb$prob, .f=rbinom)
pmap(tb, rbinom)
df
tb
pmap_df(tb, rbinom)
pmap_dfr(tb, rbinom)
pmap_df(tb, rbinom)
pmap_dfc(tb, rbinom)
pmap_dfr(tb, rbinom)
pmap_dfc(tb, rbinom)
tb
tb <- tibble(n=c(rep_len(x=5, length.out=5), rep_len(x=10, length.out=15)), size=c(rep_len(x=25, length.out=5), rep_len(x=100, length.out=15)), prob=probs)
tb <- tibble(n=c(rep_len(x=5, length.out=5), rep_len(x=10, length.out=15)), size=c(rep_len(x=25, length.out=5), rep_len(x=100, length.out=15)), prob=rep(probs, 4))
tb
pmap_dfc(tb, rbinom)
tb
pmap_dfr(tb, rbinom)
pmap_df(tb, rbinom)
pmap_dfc(tb, rbinom)
tb
tb |> as.data.frame()
tb |> as.data.frame() -> blah
blah
pmap_dfc(blah, rbinom)
pmap(blah, rbinom)
tb <- tibble(n=rep_len(x=5, length.out=5), size=rep_len(x=25, length.out=5), prob=probs)
pmap(tb, rbinom)
pmap_dfc(tb, rbinom)
pmap_dfr(tb, rbinom)
pmap_dfc(tb, rbinom)
pmap_dfc(tb, rbinom) |> t()
sample(x=2:5, size=1)
row.idc <- 1:5
row.idc
rev(row.idc)
pmap_dfr(tb, rbinom, .id="blah")
pmap_dfr(tb, rbinom, .id=NULL)
tb
row.names(tb, c(blah))
row.names(tb, "blah")
?row.names
row.names(tb)
row.names(tb) <- paste0("blah", 1:5)
tb
as.data.frame(tb) -> blah
blah
pmap_dfr(blah, rbinom, .id=NULL)
pmap_dfr(blah, rbinom)
pmap_dfr(blah, rbinom, .id="yad")
map_dfr(blah, rbinom, .id="yad")
pmap_dfr(blah, rbinom)
pmap_dfc(blah, rbinom)
pmap_dfc(blah, rbinom) |> colMeans()
tb <- tibble(n=rep_len(x=15, length.out=15), size=rep_len(x=25, length.out=15), prob=probs)
tb <- tibble(n=rep_len(x=15, length.out=15), size=rep_len(x=25, length.out=15), prob=rep(probs,3 ))
tb
pmap_dfc(tb, rbinom)
pmap_dfc(tb, rbinom)
tb
pmap_dfr(tb, rbinom)
pmap_dfc(tb, rbinom)
pmap_dfc(tb, rbinom, .id="stu")
pmap_dfc(tb, rbinom, "stu")
pmap_dfc(tb, rbinom)
tb
pmap_dfc(as.data.frame(tb), rbinom)
map2_dfr(tb$n, tb$size, tb$prob, rbinom)
map2_dfc(tb$n, tb$size, tb$prob, rbinom)
pmap_dfr(tb$n, tb$size, tb$prob, rbinom)
pmap_dfr(tb, rbinom)
pmap_dfr(t(tb), rbinom)
t(tb)



row.names(tb, "blah")
?row.names
row.names(tb)
row.names(tb) <- paste0("blah", 1:5)
tb
as.data.frame(tb) -> blah
blah
pmap_dfr(blah, rbinom, .id=NULL)
pmap_dfr(blah, rbinom)
pmap_dfr(blah, rbinom, .id="yad")
map_dfr(blah, rbinom, .id="yad")
pmap_dfr(blah, rbinom)
pmap_dfc(blah, rbinom)
pmap_dfc(blah, rbinom) |> colMeans()
tb <- tibble(n=rep_len(x=15, length.out=15), size=rep_len(x=25, length.out=15), prob=probs)
tb <- tibble(n=rep_len(x=15, length.out=15), size=rep_len(x=25, length.out=15), prob=rep(probs,3 ))
tb
pmap_dfc(tb, rbinom)
pmap_dfc(tb, rbinom)
tb
pmap_dfr(tb, rbinom)
pmap_dfc(tb, rbinom)
pmap_dfc(tb, rbinom, .id="stu")
pmap_dfc(tb, rbinom, "stu")
pmap_dfc(tb, rbinom)
tb
pmap_dfc(as.data.frame(tb), rbinom)
map2_dfr(tb$n, tb$size, tb$prob, rbinom)
map2_dfc(tb$n, tb$size, tb$prob, rbinom)
pmap_dfr(tb$n, tb$size, tb$prob, rbinom)
pmap_dfr(tb, rbinom)
pmap_dfr(t(tb), rbinom)
t(tb)
pmap_dfr(tb, rbinom)
tb
tb <- tibble(n=rep_len(x=15, length.out=15), size=rep_len(x=25, length.out=15), prob=rep(probs,3 ), id=paste0("id",1:15))
tb
pmap_dfr(tb, rbinom)
pmap_dfr(tb, rbinom, .id="id")
pmap_dfr(tb, rbinom, .id=id)
tb
column_to_rownames(tb, var="id")
column_to_rownames(tb, var="id") -> tb2
pmap_dfr(tb2, rbinom)
pmap_dfr(tb, rbinom, .id=
"yaddy")
pmap_dfr(tb, rbinom, .id="yaddy")
tb
tb <- tb[, -4]
tb
pmap_dfr(tb, rbinom, .id="yaddy")
rbinom(n=7, size=100, prob=c(0,0.5,1))

