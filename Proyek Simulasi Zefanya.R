library(simmer)
library(dplyr)
library(ggplot2)
env <- simmer("sistem TV")

set.seed(28)

diterima = function(){
  ifelse(runif(1)<0.85, return(1), return(2))}
AK = function(){round(runif(1, min = 30, max = 35),3)}
AK2 = function(){round(runif(1, min = 3.5, max = 7.5), 3)}
Lama_aktivitas = function(){round(runif(1, min = 6, max = 12),3)}
Lama_servis = function(){round(runif(1, min = 20, max = 40), 3)}

lintasan_diterima <- trajectory() %>%
  log_("TV diterima...")

lintasan_selesai <- trajectory() %>%
  log_("TV sudah selesai diperiksa")

antrian <- trajectory() %>%
  log_("kumpulan TV masuk...") %>%
  seize("inspektor") %>%
  log_("terjadi pengecekan") %>%
  timeout(Lama_aktivitas) %>%
  release("inspektor") %>%
  log_("selesai diperiksa oleh inspektor...")

lintasan_ditolak <- trajectory() %>%
  set_attribute("mengulang",1, mod = "+", init = 0) %>%
  log_("TV perlu diservis...") %>%
  seize("tukang_servis") %>%
  timeout(Lama_servis) %>%
  release("tukang_servis") %>%
  log_("kembali ke lintasan antrian...") %>%
  join(antrian)

lintasan_selesai <- trajectory() %>%
  log_("TV sudah selesai diperiksa")

antrian %>%
  branch(
    diterima,
    continue = FALSE,
    join(lintasan_diterima) %>%
      join(lintasan_selesai),
    join(lintasan_ditolak))

traj_A = trajectory() %>%
  log_("TV jenis 1 tiba") %>%
  set_attribute("jenis",1) %>%
  set_attribute("tiba", function(){now(env)}) %>%
  join(antrian)

traj_B = trajectory() %>%
  log_("TV jenis 2 tiba") %>%
  set_attribute("jenis",2) %>%
  set_attribute("tiba", function(){now(env)}) %>%
  join(antrian)

env %>%
  add_resource("inspektor", 2, preemptive = TRUE) %>%
  add_resource("tukang_servis", 1, preemptive = TRUE) %>%
  add_generator("TV_jenis_1 ", traj_A, AK, mon = 2, priority = 1) %>%
  add_generator("TV_jenis_2 ", traj_B, AK2, mon = 2) %>%
  run(500) %>% invisible

data_waktu = get_mon_arrivals(env) %>%
  transform(time_in_system = get_mon_arrivals(env)$end_time-get_mon_arrivals(env)$start_time) %>%
  .[order(.$start_time),]

data_atribut = get_mon_attributes(env)

data_atribut_jenis = data_atribut %>%
  filter(key == "jenis") %>%
  arrange(time)

data_gabungan = left_join(data_waktu, data_atribut_jenis) %>%
  arrange(value)

data_jenis_1 <- data_gabungan %>%
  filter(value == 1)

data_jenis_2 <- data_gabungan %>%
  filter(value == 2)

data_server <- get_mon_resources(env)

data_inspektor <- data_server %>%
  filter(resource == "inspektor")

data_tukang_service <- data_server %>%
  filter(resource == "tukang_servis")

data_antrian_inspektor <- data_inspektor %>%
  select(server)

hist(data_waktu$activity_time, main = "Histogram activity time", col = "lightyellow",xlab = "Lama", ylab = "Frekuensi")
abline(mean(data_waktu$activity_time),0, col = "red")

hist(data_waktu$time_in_system, main = "Histogram time in sytem", 
     col = "lightblue", xlab= "Lama", ylab = "Frekuensi")
abline(mean(data_waktu$time_in_system),0, col = "red")

data <- data.frame(x = 1 : nrow(data_inspektor), y = data_inspektor$queue)
grafik_inspektor = ggplot(data, aes(x,y)) + geom_step(direction = "hv", col = "darkgreen") +
  ylab("Jumlah Antrian") + xlab("Waktu") + labs(title = "Panjang Antrian di Server Inspektor")
grafik_inspektor

data1 <- data.frame(x = 1 : nrow(data_tukang_service), y = data_tukang_service$queue)
grafik_tukang_service = ggplot(data1, aes(x,y)) + geom_step(direction = "hv", col = "darkgreen") +
  ylab("Jumlah Antrian") + xlab("Waktu") + labs(title = "Panjang Antrian di Resource Tukang Servis")
grafik_tukang_service

library(reshape)
replikasi = 10
replikasi_env <- lapply(1:replikasi, function(i){
  simmer("sistem TV") %>%
    add_resource("inspektor", 2, preemptive = TRUE) %>%
    add_resource("tukang_servis", 1, preemptive = TRUE) %>%
    add_generator("TV_jenis_1 ", traj_A, AK, mon = 2, priority = 1) %>%
    add_generator("TV_jenis_2 ", traj_B, AK2, mon = 2) %>%
    run(500) %>% invisible
})

rata_rata <- NULL
for(i in 1:replikasi){
  rata_rata = c(rata_rata,mean(get_mon_arrivals(replikasi_env[[i]])$activity_time))
}
rata_rata

data2 <- data.frame(x = 1 : replikasi, y = rata_rata)
grafik_rata_rata_replikasi = ggplot(data2, aes(x,y)) +
  geom_line(col = "blue") + ylab("Rata-Rata") + xlab("Replikasi") + labs(title = "Rata-Rata dari Masing-Masing Replikasi") +
  geom_point(col = "black")
grafik_rata_rata_replikasi

alpha = 0.05
batas_bawah = mean(rata_rata)-qnorm(0.975, lower.tail = TRUE)*(sd(rata_rata)/sqrt(replikasi))
batas_atas = mean(rata_rata)+qnorm(0.975, lower.tail = TRUE)*(sd(rata_rata)/sqrt(replikasi))

batas_bawah_var = ((replikasi- 1)*var(rata_rata))/(qchisq(1-(alpha/2),df = replikasi-1,
                                                          lower.tail = TRUE))

batas_atas_var =  ((replikasi - 1)*var(rata_rata))/(qchisq(alpha/2,
                                                           df = replikasi - 1,
                                                           lower.tail = TRUE))

paste("Diduga parameter rata-rata berada diantara: ", batas_bawah, "dan", batas_atas)
paste("Diduga parameter variansi berada diantara: ", batas_bawah_var, "dan", batas_atas_var)

paste("rata rata antrian pada tukang servis adalah:", mean(data_tukang_service$queue))

paste("antrian terpanjang adalah:",max(data_inspektor$queue))


