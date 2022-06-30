// This source code is subject to the terms of the Mozilla Public License 2.0 at https://mozilla.org/MPL/2.0/
// © wielkieef

//@version=4


strategy(title = "Bommbarding BTC [30min]", overlay = true, initial_capital = 1000, pyramiding = 25, calc_on_order_fills = false, calc_on_every_tick = false, default_qty_type = strategy.percent_of_equity, default_qty_value = 50, commission_value = 0.03)
 
//SOURCE =============================================================================================================================================================================================================================================================================================================

src                 =                   input(open)

// INPUTS ============================================================================================================================================================================================================================================================================================================

length = input(13)
gamma = input(3)
zl = input(true, title="Zero-Lag")


//RSI----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

len_3               =                   input(61,                               title="RSI lenght",                                                                                                                         group = "Relative Strenght Indeks")
src_3               =                   input(close,                            title="RSI Source",                                                                                                                         group = "Relative Strenght Indeks")
RSI_VWAP_length     =                   input(14,                                title="Rsi vwap lenght")

//ADX-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ADX_options         =                   input("CLASSIC",                        title="  Adx Type",                                          options = ["CLASSIC", "MASANAKAMURA"],                                          group="Average Directional Index")
ADX_len             =                   input(15,                               title="  Adx Lenght",                                     type=input.integer, minval = 1,                                                     group="Average Directional Index")
th                  =                   input(10,                               title="  Adx Treshold",                                   type=input.integer, minval = 0,                                                     group="Average Directional Index")

// Volume ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

volume_f            =                   input(1.6,                              title="Volume mult.",                                       minval = 0, step = 0.1,                                                         group="Volume")
sma_length          =                   input(31,                               title="Volume lenght",                                      minval = 1,                                                                     group="Volume")

//SAR----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Sst                 =                   input (0.3,                             title="Sar Start",                                          step=0.01, minval = 0.01,                                                       group="SAR")
Sinc                =                   input (0.3,                             title="Sar Int",                                            step=0.01, minval = 0.01,                                                       group="SAR")
Smax                =                   input (0.35,                             title="Sar Max",                                            step=0.01, minval = 0.01,                                                       group="SAR")

//MACD----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fast_length         =                   input(5,                               title="  Fast Length",                                        type=input.integer,                                                             group="MACD")
slow_length         =                   input(12,                               title="  Slow Length",                                        type=input.integer,                                                             group="MACD")
signal_length       =                   input(8,                               title="  Signal Smoothing",                                   type=input.integer,                                                             group="MACD")

//FLAT ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ma = 0.
mad = 0.
//----
change_1 = change(close, length / 2)
src_ = zl ? close + change_1 : close
ma := nz(mad[1], src_)
d = cum(abs(src_[length] - ma)) / bar_index * gamma
mad := sma(sma(src_ > nz(mad[1], src_) + d ? src_ + d : src_ < nz(mad[1], src_) - d ? src_ - d : nz(mad[1], src_), length), length)
//----
plot(mad, color=color.red, linewidth=2, transp=0)
mad_flat = mad/mad[1] > .999 and mad/mad[1] < 1.001

//INDICATORS =======================================================================================================================================================================================================================================================================================================

//ADX-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

calcADX(_len) =>
    up              =                                                                                                                       change(high)
	down            =                                                                                                                      -change(low)
	plusDM          =                                                                                                                       na(up)   ? na : (up > down and up > 0   ? up   : 0)
    minusDM         =                                                                                                                       na(down) ? na : (down > up and down > 0 ? down : 0)
	truerange       =                                                                                                                       rma(tr, _len)
	_plus           =                                                                                                                       fixnan(100 * rma(plusDM, _len)  / truerange)
	_minus          =                                                                                                                       fixnan(100 * rma(minusDM, _len) / truerange)
	sum             =                                                                                                                       _plus + _minus
	_adx            =                                                                                                                       100 * rma(abs(_plus - _minus) / (sum == 0 ? 1 : sum), _len)
    [_plus,_minus,_adx]
calcADX_Masanakamura(_len) =>
    SmoothedTrueRange                   =                                                                                                   0.0
    SmoothedDirectionalMovementPlus     =                                                                                                   0.0
    SmoothedDirectionalMovementMinus    =                                                                                                   0.0
    TrueRange                           =                                                                                                   max(max(high - low, abs(high - nz(close[1]))), abs(low - nz(close[1])))
    DirectionalMovementPlus             =                                                                                                   high - nz(high[1]) > nz(low[1]) - low ? max(high - nz(high[1]), 0) : 0
    DirectionalMovementMinus            =                                                                                                   nz(low[1]) - low > high - nz(high[1]) ? max(nz(low[1]) - low, 0)   : 0
    SmoothedTrueRange                   :=                                                                                                  nz(SmoothedTrueRange[1]) - (nz(SmoothedTrueRange[1]) /_len) + TrueRange
    SmoothedDirectionalMovementPlus     :=                                                                                                  nz(SmoothedDirectionalMovementPlus[1])  - (nz(SmoothedDirectionalMovementPlus[1])  / _len) + DirectionalMovementPlus
    SmoothedDirectionalMovementMinus    :=                                                                                                  nz(SmoothedDirectionalMovementMinus[1]) - (nz(SmoothedDirectionalMovementMinus[1]) / _len) + DirectionalMovementMinus
    DIP                                 =                                                                                                   SmoothedDirectionalMovementPlus  / SmoothedTrueRange * 100
    DIM                                 =                                                                                                   SmoothedDirectionalMovementMinus / SmoothedTrueRange * 100
    DX                                  =                                                                                                   abs(DIP-DIM) / (DIP+DIM)*100
    adx                                 =                                                                                                   sma(DX, _len)
    [DIP,DIM,adx]
[DIPlusC,DIMinusC,ADXC] =                                                                                                                   calcADX(ADX_len) 
[DIPlusM,DIMinusM,ADXM] =                                                                                                                   calcADX_Masanakamura(ADX_len)

DIPlus                  =                                                                                                                   ADX_options == "CLASSIC" ? DIPlusC    : DIPlusM
DIMinus                 =                                                                                                                   ADX_options == "CLASSIC" ? DIMinusC   : DIMinusM
ADX                     =                                                                                                                   ADX_options == "CLASSIC" ? ADXC       : ADXM
L_adx                   =                                                       DIPlus > DIMinus and ADX > th
S_adx                   =                                                       DIPlus < DIMinus and ADX > th

//SAR------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

SAR                     =                                                                                                                   sar(Sst, Sinc, Smax)
L_sar                   =                                                       (SAR < close)
S_sar                   =                                                       (SAR > close)

// Volume -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Volume_condt            =                                                       volume > sma(volume,sma_length)*volume_f


//RSI------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

up_3                    =                                                                                                                   rma(max(change(src_3), 0), len_3)
down_3                  =                                                                                                                   rma(-min(change(src_3), 0), len_3)
rsi_3                   =                                                                                                                   down_3 == 0 ? 100 : up_3 == 0 ? 0 : 100 - (100 / (1 + up_3 / down_3))
L_rsi                   =                                                       (rsi_3 < 70)
S_rsi                   =                                                       (rsi_3 > 30) 
RSI_VWAP                = rsi(vwap(close), RSI_VWAP_length)
RSI_VWAP_overSold       = 13
RSI_VWAP_overBought     = 68

L_VAP                   =                                                       (crossover(RSI_VWAP, RSI_VWAP_overSold)) and close < mad
S_VAP                   =                                                       (crossunder(RSI_VWAP, RSI_VWAP_overBought)) and close > mad

//MACD-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fast_ma                 =                                                                                                                   ema(src, fast_length)
slow_ma                 =                                                                                                                   ema(src, slow_length)
macd                    =                                                                                                                   fast_ma - slow_ma
signal_                 =                                                                                                                   sma(macd, signal_length)
L_macd                  =                                                       macd > signal_ 
S_macd                  =                                                       macd < signal_ 

//STRATEGY ==========================================================================================================================================================================================================================================================================================================



L_1     =                                                                       mad_flat and L_VAP and L_sar
S_1     =                                                                       mad_flat and S_VAP and S_sar

L_2     =                                                                       L_adx and  L_sar and Volume_condt and L_rsi and L_macd and not mad_flat
S_2     =                                                                       S_adx and  S_sar and Volume_condt and S_rsi and S_macd and not mad_flat

L_3         =                                                                   L_adx and not (L_1 or L_2 )
S_3         =                                                                   S_adx  and not (L_1 or S_2 )

L_basic_condt       =     L_1    or L_2
S_basic_condt       =      S_1   or S_2

var bool longCondition          = na
var bool shortCondition         = na
var float last_open_longCondition = na
var float last_open_shortCondition = na
var int last_longCondition = 0
var int last_shortCondition = 0
longCondition := L_basic_condt
shortCondition := S_basic_condt
last_open_longCondition := longCondition ? close : nz(last_open_longCondition[1])
last_open_shortCondition := shortCondition ? close : nz(last_open_shortCondition[1])
last_longCondition := longCondition ? time : nz(last_longCondition[1])
last_shortCondition := shortCondition ? time : nz(last_shortCondition[1])
in_longCondition = last_longCondition > last_shortCondition
in_shortCondition = last_shortCondition > last_longCondition

// SWAP-SL ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

var int last_long_sl = na
var int last_short_sl = na
sl = input(2, "Swap % period", type = input.float, minval = 0, step = 0.1, group="strategy settings")
long_sl = crossunder(low, (1-(sl/100))*last_open_longCondition) and in_longCondition and not longCondition
short_sl = crossover(high, (1+(sl/100))*last_open_shortCondition) and in_shortCondition and not shortCondition
last_long_sl := long_sl ? time : nz(last_long_sl[1])
last_short_sl := short_sl ? time : nz(last_short_sl[1])
var bool CondIni_long_sl = 0
CondIni_long_sl := long_sl ? 1 : longCondition ? -1 : nz(CondIni_long_sl[1])
var bool CondIni_short_sl = 0
CondIni_short_sl := short_sl ? 1 : shortCondition ? -1 : nz(CondIni_short_sl[1])
Final_Long_sl = long_sl and nz(CondIni_long_sl[1]) == -1 and in_longCondition and not longCondition
Final_Short_sl = short_sl and nz(CondIni_short_sl[1]) == -1 and in_shortCondition and not shortCondition
var int sectionLongs = 0
sectionLongs := nz(sectionLongs[1])
var int sectionShorts = 0
sectionShorts := nz(sectionShorts[1])

// RE-ENTRY ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

if longCondition or Final_Long_sl
    sectionLongs := sectionLongs + 1
    sectionShorts := 0
if shortCondition or Final_Short_sl
    sectionLongs := 0
    sectionShorts := sectionShorts + 1
var float sum_long = 0.0
var float sum_short = 0.0

if longCondition
    sum_long := nz(last_open_longCondition) + nz(sum_long[1])
    sum_short := 0.0
if Final_Long_sl
    sum_long := ((1-(sl/100))*last_open_longCondition) + nz(sum_long[1])
    sum_short := 0.0
if shortCondition
    sum_short := nz(last_open_shortCondition) + nz(sum_short[1])
    sum_long := 0.0
if Final_Short_sl
    sum_long := 0.0
    sum_short := ((1+(sl/100))*last_open_shortCondition) + nz(sum_short[1])
   
var float Position_Price = 0.0
Position_Price := nz(Position_Price[1])
Position_Price := longCondition or Final_Long_sl ? sum_long/(sectionLongs) : shortCondition or Final_Short_sl ? sum_short/(sectionShorts) : na

//TP_1 -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tp = input(1.5, "Tp-1 ", type = input.float, minval = 0, step = 0.1, group="strategy settings")
long_tp = crossover(high, (1+(tp/100))*fixnan(Position_Price)) and in_longCondition and not longCondition
short_tp = crossunder(low, (1-(tp/100))*fixnan(Position_Price)) and in_shortCondition and not shortCondition
var int last_long_tp = na
var int last_short_tp = na
last_long_tp := long_tp ? time : nz(last_long_tp[1])
last_short_tp := short_tp ? time : nz(last_short_tp[1])
Final_Long_tp = long_tp and last_longCondition > nz(last_long_tp[1])
Final_Short_tp = short_tp and last_shortCondition > nz(last_short_tp[1])
ltp = iff(Final_Long_tp, (fixnan(Position_Price)*(1+(tp/100))), na)
stp = iff(Final_Short_tp, (fixnan(Position_Price)*(1-(tp/100))), na)
if Final_Short_tp or Final_Long_tp
    sum_long := 0.0
    sum_short := 0.0
    sectionLongs := 0
    sectionShorts := 0
if Final_Long_tp
    CondIni_long_sl == 1
if Final_Short_tp
    CondIni_short_sl == 1
    
//TP_2 ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tp2 = input(2.8, "Tp-2 ", type = input.float, minval = 0, step = 0.1, group="strategy settings")
long_tp2 = crossover(high, (1+(tp2/100))*fixnan(Position_Price)) and in_longCondition and not longCondition
short_tp2 = crossunder(low, (1-(tp2/100))*fixnan(Position_Price)) and in_shortCondition and not shortCondition
var int last_long_tp2 = na
var int last_short_tp2 = na
last_long_tp2 := long_tp2 ? time : nz(last_long_tp2[1])
last_short_tp2 := short_tp2 ? time : nz(last_short_tp2[1])
Final_Long_tp2 = long_tp2 and last_longCondition > nz(last_long_tp2[1])
Final_Short_tp2 = short_tp2 and last_shortCondition > nz(last_short_tp2[1])
CondIni_long_sl := long_sl or Final_Long_tp ? 1 : longCondition ? -1 : nz(CondIni_long_sl[1])
CondIni_short_sl := short_sl or Final_Short_tp? 1 : shortCondition ? -1 : nz(CondIni_short_sl[1])
ltp2 = iff(Final_Long_tp2, (fixnan(Position_Price)*(1+(tp2/100))), na)
stp2 = iff(Final_Short_tp2, (fixnan(Position_Price)*(1-(tp2/100))), na)

// COLORS & PLOTS --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

bar_color = L_adx ? #009688 : S_adx ? #f06292 : color.gray

barcolor                                                                        (color = bar_color)


plotshape(longCondition,            title="Long",                   style=shape.triangleup,                 location=location.belowbar,                         color=color.blue,           size=size.tiny ,                                                           transp = 0                  )
plotshape(shortCondition,           title="Short",                  style=shape.triangledown,               location=location.abovebar,                         color=color.red,            size=size.tiny ,                                                           transp = 0                  )

plot(ltp2, style=plot.style_cross, linewidth=6, color = color.fuchsia, editable = false)
plot(stp2, style=plot.style_cross, linewidth=6, color = color.fuchsia, editable = false)
plot(ltp, style=plot.style_cross, linewidth=6, color = color.fuchsia, editable = false)
plot(stp, style=plot.style_cross, linewidth=6, color = color.fuchsia, editable = false)

//BACKTESTING--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Q = 50

strategy.entry("long", strategy.long, when = (longCondition ))
strategy.entry("short", strategy.short, when = (shortCondition ))

strategy.exit("TP", "long", qty_percent = Q, limit = (fixnan(Position_Price)*(1+(tp/100))))
strategy.exit("TP", "short", qty_percent = Q, limit = (fixnan(Position_Price)*(1-(tp/100))))

strategy.exit("TP2", "long", qty_percent = abs(100-Q), limit = (fixnan(Position_Price)*(1+(tp2/100))))
strategy.exit("TP2", "short", qty_percent = abs(100-Q), limit = (fixnan(Position_Price)*(1-(tp2/100))))







//
//
//
//
//
//

// By wielkieef




