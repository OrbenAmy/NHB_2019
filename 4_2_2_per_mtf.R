##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 4.2.2: Combining MTF permutations run over the computer cluster
##########################################################################################
library(tidyverse)
library(data.table)

##########################################################################################
# Load data
##########################################################################################
setwd(".../4_per/Output_Rename")

#######################################################
# Load bootstrapped analyses and merge
#######################################################
iterations <- 500
specifications <- 40950

for (i in 1:iterations){
assign(paste0("per_", i), read.csv(paste0("mtf_permutation_", i, ".csv"), nrow=2))
}

per <- do.call("rbind", list(per_1,per_2,per_3,per_4,per_5,per_6,per_7,per_8,per_9,per_10,per_11,per_12,per_13,per_14,per_15,per_16,
                             per_17,per_18,per_19,per_20,per_21,per_22,per_23,per_24,per_25,per_26,per_27,per_28,per_29,per_30,per_31,per_32, 
                             per_33,per_34,per_35,per_36,per_37,per_38,per_39,per_40,per_41,per_42,per_43,per_44,per_45,per_46,per_47,per_48, 
                             per_49,per_50,per_51,per_52,per_53,per_54,per_55,per_56,per_57,per_58,per_59,per_60,per_61,per_62,per_63,per_64, 
                             per_65,per_66,per_67,per_68,per_69,per_70,per_71,per_72,per_73,per_74,per_75,per_76,per_77,per_78,per_79,per_80, 
                             per_81,per_82,per_83,per_84,per_85,per_86,per_87,per_88,per_89,per_90,per_91,per_92,per_93,per_94,per_95,per_96,
                             per_97,per_98,per_99,per_100,per_101,per_102,per_103,per_104,per_105,per_106,per_107,per_108,per_109,per_110,per_111,per_112,
                             per_113,per_114,per_115,per_116,per_117,per_118,per_119,per_120,per_121,per_122,per_123,per_124,per_125,per_126,per_127,per_128,
                             per_129,per_130,per_131,per_132,per_133,per_134,per_135,per_136,per_137,per_138,per_139,per_140,per_141,per_142,per_143,per_144,
                             per_145,per_146,per_147,per_148,per_149,per_150,per_151,per_152,per_153,per_154,per_155,per_156,per_157,per_158,per_159,per_160,
                             per_161,per_162,per_163,per_164,per_165,per_166,per_167,per_168,per_169,per_170,per_171,per_172,per_173,per_174,per_175,per_176,
                             per_177,per_178,per_179,per_180,per_181,per_182,per_183,per_184,per_185,per_186,per_187,per_188,per_189,per_190,per_191,per_192,
                             per_193,per_194,per_195,per_196,per_197,per_198,per_199,per_200,per_201,per_202,per_203,per_204,per_205,per_206,per_207,per_208,
                             per_209,per_210,per_211,per_212,per_213,per_214,per_215,per_216,per_217,per_218,per_219,per_220,per_221,per_222,per_223,per_224,
                             per_225,per_226,per_227,per_228,per_229,per_230,per_231,per_232,per_233,per_234,per_235,per_236,per_237,per_238,per_239,per_240,
                             per_241,per_242,per_243,per_244,per_245,per_246,per_247,per_248,per_249,per_250,per_251,per_252,per_253,per_254,per_255,per_256,
                             per_257, per_258,per_259,per_260,per_261,per_262,per_263,per_264,per_265,per_266,
                             per_267,per_268,per_269,per_270,per_271,per_272,per_273,per_274,per_275,per_276,per_277,per_278,per_279,per_280,per_281,per_282,
                             per_283,per_284,per_285,per_286,per_287,per_288,per_289,per_290,per_291,per_292,per_293,per_294,per_295,per_296,per_297,per_298,
                             per_299,per_300,per_301,per_302,per_303,per_304,per_305,per_306,per_307,per_308,per_309,per_310,per_311,per_312,per_313,per_314,
                             per_315,per_316,per_317,per_318,per_319,per_320,per_321,per_322,per_323,per_324,per_325,per_326,per_327,per_328,per_329,per_330,
                             per_331,per_332,per_333,per_334,per_335,per_336,per_337,per_338,per_339,per_340,per_341,per_342,per_343,per_344,per_345,per_346,
                             per_347,per_348,per_349,per_350,per_351,per_352,per_353,per_354,per_355,per_356,per_357,per_358,per_359,per_360,per_361,per_362,
                             per_363,per_364,per_365,per_366,per_367,per_368,per_369,per_370,per_371,per_372,per_373,per_374,per_375,per_376,per_377,per_378,
                             per_379,per_380,per_381,per_382,per_383,per_384,per_385,per_386,per_387,per_388,per_389,per_390,per_391,per_392,per_393,per_394,
                             per_395,per_396,per_397,per_398,per_399,per_400,per_401,per_402,per_403,per_404,per_405,per_406,per_407,per_408,per_409,per_410,
                             per_411,per_412,per_413,per_414,per_415,per_416,per_417,per_418,per_419,per_420,per_421,per_422,per_423,per_424,per_425,per_426,
                             per_427,per_428,per_429,per_430,per_431,per_432,per_433,per_434,per_435,per_436,per_437,per_438,per_439,per_440,per_441,per_442,
                             per_443,per_444,per_445,per_446,per_447,per_448,per_449,per_450,per_451,per_452,per_453,per_454,per_455,per_456,per_457,per_458,
                             per_459,per_460,per_461,per_462,per_463,per_464,per_465,per_466,per_467,per_468,per_469,per_470,per_471,per_472,per_473,per_474,
                             per_475,per_476,per_477,per_478,per_479,per_480,per_481,per_482,per_483,per_484,per_485,per_486,per_487,per_488,per_489,per_490,
                             per_491,per_492,per_493,per_494,per_495,per_496,per_497,per_498,per_499,per_500))

#######################################################
# Load original results frame
#######################################################
setwd(".../2_sca")
load("2_2_sca_mtf_results_subset1.rda")
results_mtf_sca <- results_mtf_ds_1

results_mtf_sca_cont <- results_mtf_sca %>% filter(controls == "Controls")
results_mtf_sca_ncont <- results_mtf_sca %>% filter(controls == "No Controls")

results_mtf_sca_sig <- results_mtf_sca %>% filter(p_value < 0.05)
results_mtf_sca_cont_sig <- results_mtf_sca %>% filter(controls == "Controls" & p_value < 0.05)
results_mtf_sca_ncont_sig <- results_mtf_sca %>% filter(controls == "No Controls" & p_value < 0.05)

### 1. effect sizes
mean.obs <- mean(results_mtf_sca$effect, na.rm = TRUE)
mean.obs.c <- mean(results_mtf_sca_cont$effect, na.rm = TRUE)
mean.obs.nc <- mean(results_mtf_sca_ncont$effect, na.rm = TRUE)

### 2. sign of effect 
sign.boot <- pmax(per$sign.neg.boot, per$sign.pos.boot)
sign.boot.c <- pmax(per$sign.neg.boot.c, per$sign.pos.boot.c)
sign.boot.nc <- pmax(per$sign.neg.boot.nc, per$sign.pos.boot.nc)
sign.obs <- pmax(table(sign(results_mtf_sca$effect))[[-1]],table(sign(results_mtf_sca$effect))[[1]])
sign.obs.c <- pmax(table(sign(results_mtf_sca_cont$effect))[[-1]],table(sign(results_mtf_sca_cont$effect))[[1]])
sign.obs.nc <- pmax(table(sign(results_mtf_sca_ncont$effect))[[-1]],table(sign(results_mtf_sca_ncont$effect))[[1]])

### 3. sign of significant effects
sign.sig.boot <- pmax(per$sign.sig.neg.boot, per$sign.sig.pos.boot)
sign.sig.boot.c <- pmax(per$sign.sig.neg.boot.c, per$sign.sig.pos.boot.c)
sign.sig.boot.nc <- pmax(per$sign.sig.neg.boot.nc, per$sign.sig.pos.boot.nc)
sign.sig.obs <- pmax(table(sign(results_mtf_sca_sig$effect))[[-1]],table(sign(results_mtf_sca_sig$effect))[[1]])
sign.sig.obs.c <- pmax(table(sign(results_mtf_sca_cont_sig$effect))[[-1]],table(sign(results_mtf_sca_cont_sig$effect))[[1]])
sign.sig.obs.nc <- pmax(table(sign(results_mtf_sca_ncont_sig$effect))[[-1]],table(sign(results_mtf_sca_ncont_sig$effect))[[1]])

#######################################################
# Analyses
#######################################################
p1=mean(abs(per$effect.boot)>=abs(mean.obs))
p1.c=mean(abs(per$effect.boot.c)>=abs(mean.obs.c))
p1.nc=mean(abs(per$effect.boot.nc)>=abs(mean.obs.nc))

p2=mean(sign.boot>=sign.obs)
p2.c=mean(sign.boot.c>=sign.obs.c)
p2.nc=mean(sign.boot.nc>=sign.obs.nc)

p3=mean(sign.sig.boot>=sign.sig.obs)
p3.c=mean(sign.sig.boot.c>=sign.sig.obs.nc)
p3.nc=mean(sign.sig.boot.c>=sign.sig.obs.nc)

