library(apettr)
get_trips()

# Already Coded -----------------------------------------------------------
#coded annotation files
data_coded_files <- ezlist.files("C:\\Users\\apett\\Dropbox (AgeLab)\\Projects\\AVT_ST_Glance_Dataset_102020\\data\\Cadillac\\annotations_10Hz\\glance_annotations","csv") %>%
  mutate(clip_id = str_extract(file,"\\d+_\\d+(?=_glance)"),
         trip_id = str_extract(clip_id,"^\\d+") %>% as.numeric()) %>%
  left_join(trips %>% select(trip_id,trip_label_share,subject_id))

#source video files
video_coded_files <- ezlist.files("E:/avt/clips_for_analysis","mp4$",T) %>%
  filter(str_detect(tolower(in_path),"ct6|cadillac"),
         !str_detect(tolower(in_path),"cadillac_ttc_baselines")) %>%
  mutate(clip_id = str_extract(file,"\\d+_b?\\d+$") %>% str_remove("b")) %>%
  filter(clip_id %in% data_coded_files$clip_id) %>%
  group_by(clip_id) %>%
  mutate(n_vids=n()) %>%
  ungroup()

#filter coded to just those files which have 2-panel and 4-panel vids
#Not a requirement, just easier to work with
to_load <- data_coded_files %>%
  filter(clip_id %in% filter(video_coded_files,n_vids==2)$clip_id)

#remove epochs more likely to be difficult to annotate
data_load <- pblapply(1:nrow(to_load),function(i){
  data <- read_csv(to_load$in_path[i],col_types = '-c', progress = F) %>%
    distinct()
  if(any(data$label %in% c("Eyes Closed",
                           "No Eyes Visible",
                           "not_annotatable",
                           "not_annotated")))return(NULL)
  return(to_load[i,])
}) %>% bind_rows()

#Random sample of 200 epochs, weighted by subject trips
data_random <- data_load %>%
  group_by(subject_id) %>%
  mutate(n = n()) %>%
  filter(n>=12) %>%
  slice_sample(n=12)

video_random <- video_coded_files %>%
  filter(clip_id %in% data_random$clip_id) %>%
  separate(clip_id,c("trip_id","frame"),"_",F,T) %>%
  left_join(select(trips,trip_id,trip_label_share)) %>%
  mutate(panels=if_else(str_detect(in_path,"reclip"),2,4),
         out_path = paste0("C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances/videos/",
                           panels,"-panel/",trip_label_share,"_",frame,".mp4")) %>%
  select(trip_label_share,clip_id,in_path,out_path)


# Copy Sample Files -------------------------------------------------------

#annotations
file.copy(data_random$in_path,"C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\annotation/seeing_machines_cadillac_glances/previous_10Hz")

#videos
file.copy(video_random$in_path,video_random$out_path)

#can
can_random <- data_random %>%
  transmute(in_path = paste0("C:\\Users\\apett\\Dropbox (AgeLab)\\Projects\\AVT_ST_Glance_Dataset_102020\\data\\Cadillac\\CAN_epochs_10Hz/",
                             str_replace(file,"glance","can"),".csv"))

file.copy(can_random$in_path,"C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances/can_10Hz")

