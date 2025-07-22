library(ggplot2)
library(hexSticker)

imgurl <- "C:/Users/Administrator/Dropbox/Shiny hiv/shinyhiv/man/figures/hex.jpg"
library(ggthemes)
sticker(imgurl, package = "HIViz",
        s_width = .62,
        p_size = 50,
        p_y = -0.7,
        p_family = "serif",
        p_color = "#000080",
        p_fontface = "bold",
        #h_fill = "white", # رنگ پس‌زمینه استیکر
        s_x = 1, # مقیاس افقی
        s_y = 1, # مقیاس عمودی# بزرگ‌تر کردن تصویر برای فیت شدن
        h_fill = "white",  # رنگ پس‌زمینه)
        h_color = "#AD86C6", # رنگ حاشیه (نارنجی)
        bg = "transparent",
        filename = "C:/Users/Administrator/Dropbox/Shiny hiv/shinyhiv/man/figures/hex.png")

"#BD6C99"
"#D54A45"
"#FFFF00"
"#FF5500"
"#b07aa1"
"#a10000"
"#92278F"