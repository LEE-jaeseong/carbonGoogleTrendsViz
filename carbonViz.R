setwd("C://Users/owner/Documents/research/210504탄소중립화검색데이터시각화")
#install.packages("igraph")
#install.packages("tidygraph")
#install.packages("ggraph")
library(igraph)
library(tidygraph)
library(ggraph)

##### 연관어

relatedTermIdx = grep("연관어",list.files())

relatedTermDoc = list.files()[relatedTermIdx]

### 개별 파일
for (i in 1:length(relatedTermDoc)) {
  dataLoad = read.csv(relatedTermDoc[i])
  g = graph_from_data_frame(dataLoad)
  gTbl = as_tbl_graph(g)
  
  png(filename = paste0(strsplit(relatedTermDoc[i], split = ".csv")[[1]],'.png'), width=800, height=600)
  
  print(ggraph(gTbl, layout = "graphopt") + 
          geom_node_point() + 
          geom_edge_link(aes(width = dataLoad$v/10), alpha = 0.8) + 
          scale_edge_width(range = c(0.2,2)) + 
          geom_node_text(aes(label = V(g)$name), repel = TRUE) + 
          labs(edge_width = "인기도") +
          theme_graph())
  
  dev.off()
  cat("iteration: ", i, "/", length(relatedTermDoc),"\n")
}

### 전체 파일
dataTot = list()
for (j in 1:length(relatedTermDoc)) {
  dataTot[[j]] = read.csv(relatedTermDoc[j])
}

dataTotOutput = do.call(rbind, dataTot)
dataTotOutput = na.omit(dataTotOutput)

g = graph_from_data_frame(dataTotOutput)
gTbl = as_tbl_graph()

gTbl2 = dataTotOutput %>% as_tbl_graph() %>% mutate(degree = centrality_degree())

png(filename = '데이터전체연결중심성분석.png', width=800, height=600)

print(ggraph(gTbl2, layout = "nicely") + 
        geom_node_point(aes(color=degree,size=degree)) + 
        geom_edge_link(aes(width = dataTotOutput$v/100), alpha = 0.3) + 
        scale_edge_width(range = c(0.2,2)) + 
        geom_node_text(aes(label = V(g)$name, color=degree, size=degree+10), repel = TRUE) + 
        labs(edge_width = "인기도") +
        theme_graph())

dev.off()


gTbl3 = dataTotOutput %>% as_tbl_graph() %>% mutate(betweeness = centrality_betweenness())

png(filename = '데이터전체매개중심성분석.png', width=800, height=600)

print(ggraph(gTbl3, layout = "nicely") + 
        geom_node_point(aes(color=betweeness,size=betweeness)) + 
        geom_edge_link(aes(width = dataTotOutput$v/100), alpha = 0.3) + 
        scale_edge_width(range = c(0.2,2)) + 
        geom_node_text(aes(label = V(g)$name, color=betweeness, size=betweeness+10), repel = TRUE) + 
        labs(edge_width = "인기도") +
        theme_graph())

dev.off()


##### 시계열
library(lubridate)
trendIdx = grep("트렌드",list.files())

trendDoc = list.files()[trendIdx]

for (k in 1:length(trendDoc)) {
  dataLoad = read.csv(trendDoc[k])
  date = rownames(dataLoad)[-1]
  date = as_date(date)
  volume = dataLoad[-1,]
  
  dataPrep = data.frame(date, volume)
  
  png(filename = paste0(strsplit(trendDoc[k], split = ".csv")[[1]],'.png'), width=800, height=600)
  
  #ggplot(data=dataPrep, aes(x=date, y = volume)) + geom_line(color = "#00AFBB",size=1)+theme_minimal()
  print(ggplot(data=dataPrep, aes(x=date, y = volume)) + 
          geom_area(color = "#00AFBB", fill="#00AFBB",alpha=0.5, position = position_dodge(0.8))+
          theme_minimal())
  
  dev.off()
  cat("iteration: ", k, "/", length(trendDoc),"\n")
}

