all: base

build_dir := build

# $(build_dir)/%.o: %.cc
# 	@mkdir -p $(dir $@)
# 	clang++ -std=c++11 -c -o $@ $(filter-out $(build_dir)/,$<)
# 
# $(build_dir)/%.o: %.cpp
# 	@mkdir -p $(dir $@)
# 	clang++ -std=c++11 -c -o $@ $(filter-out $(build_dir)/,$<)

$(build_dir)/%.o: %.c
	@mkdir -p $(dir $@)
	clang -I. -DABC_USE_STDINT_H=1 -c -o $@ $(filter-out $(build_dir)/,$<)

base_abc_targets := \
	abcAig.o \
    abcBarBuf.o \
    abcBlifMv.o \
    abcCheck.o \
    abcDfs.o \
    abcFanio.o \
    abcFanOrder.o \
    abcFunc.o \
    abcHie.o \
    abcHieCec.o \
    abcHieGia.o \
    abcHieNew.o \
    abcLatch.o \
    abcLib.o \
    abcMinBase.o \
    abcNames.o \
    abcNetlist.o \
    abcNtk.o \
    abcObj.o \
    abcRefs.o \
    abcShow.o \
    abcSop.o \
    abcUtil.o

base_abc: $(foreach obj,$(base_abc_targets),$(build_dir)/base/abc/$(obj))

base_abci_targets := \
	abc.o \
    abcAttach.o \
    abcAuto.o \
    abcBalance.o \
    abcBidec.o \
    abcBm.o \
    abcBmc.o \
    abcCas.o \
    abcCascade.o \
    abcCollapse.o \
    abcCut.o \
    abcDar.o \
    abcDebug.o \
    abcDec.o \
    abcDetect.o \
    abcDress.o \
    abcDress2.o \
    abcDress3.o \
    abcDsd.o \
    abcEco.o \
    abcExact.o \
    abcExtract.o \
    abcFraig.o \
    abcFx.o \
    abcFxu.o \
    abcGen.o \
    abcHaig.o \
    abcIf.o \
    abcIfif.o \
    abcIfMux.o \
    abcIvy.o \
    abcLog.o \
    abcLut.o \
    abcLutmin.o \
    abcMap.o \
    abcMerge.o \
    abcMfs.o \
    abcMini.o \
    abcMiter.o \
    abcMulti.o \
    abcNtbdd.o \
    abcNpn.o \
    abcNpnSave.o \
    abcOdc.o \
    abcOrder.o \
    abcPart.o \
    abcPrint.o \
    abcProve.o \
    abcQbf.o \
    abcQuant.o \
    abcRec3.o \
    abcReconv.o \
    abcReach.o \
    abcRefactor.o \
    abcRenode.o \
    abcReorder.o \
    abcRestruct.o \
    abcResub.o \
    abcRewrite.o \
    abcRpo.o \
    abcRr.o \
    abcRunGen.o \
    abcSat.o \
    abcSaucy.o \
    abcScorr.o \
    abcSense.o \
    abcSpeedup.o \
    abcStrash.o \
    abcSweep.o \
    abcSymm.o \
    abcTim.o \
    abcTiming.o \
    abcUnate.o \
    abcUnreach.o \
    abcVerify.o \
    abcXsim.o

base_abci: $(foreach obj,$(base_abci_targets),$(build_dir)/base/abci/$(obj))

base_cmd_targets := \
	cmd.o \
    cmdAlias.o \
    cmdApi.o \
    cmdAuto.o \
    cmdFlag.o \
    cmdHist.o \
    cmdLoad.o \
    cmdPlugin.o \
    cmdStarter.o \
    cmdUtils.o

base_cmd: $(foreach obj,$(base_cmd_targets),$(build_dir)/base/cmd/$(obj))

base_io_targets := \
	io.o \
    ioJson.o \
    ioReadAiger.o \
    ioReadBaf.o \
    ioReadBblif.o \
    ioReadBench.o \
    ioReadBlif.o \
    ioReadBlifAig.o \
    ioReadBlifMv.o \
    ioReadDsd.o \
    ioReadEdif.o \
    ioReadEqn.o \
    ioReadPla.o \
    ioReadPlaMo.o \
    ioReadVerilog.o \
    ioUtil.o \
    ioWriteAiger.o \
    ioWriteBaf.o \
    ioWriteBblif.o \
    ioWriteBench.o \
    ioWriteBlif.o \
    ioWriteBlifMv.o \
    ioWriteBook.o \
    ioWriteCnf.o \
    ioWriteDot.o \
    ioWriteEqn.o \
    ioWriteEdgelist.o \
    ioWriteGml.o \
    ioWriteList.o \
    ioWritePla.o \
    ioWriteVerilog.o \
    ioWriteSmv.o

base_io: $(foreach obj,$(base_io_targets),$(build_dir)/base/io/$(obj))

base_exor_targets := \
	exor.o \
    exorBits.o \
    exorCubes.o \
    exorLink.o \
    exorList.o \
    exorUtil.o

base_exor: $(foreach obj,$(base_exor_targets),$(build_dir)/base/exor/$(obj))

base_ver_targets := \
    verCore.o \
    verFormula.o \
    verParse.o \
    verStream.o

base_ver: $(foreach obj,$(base_ver_targets),$(build_dir)/base/ver/$(obj))

base_wlc_targets := \
	wlcAbs.o \
    wlcAbs2.o \
    wlcAbc.o \
    wlcPth.o \
    wlcBlast.o \
    wlcCom.o \
    wlcGraft.o \
    wlcJson.o \
    wlcMem.o \
    wlcNdr.o \
    wlcNtk.o \
    wlcReadSmt.o \
    wlcReadVer.o \
    wlcSim.o \
    wlcShow.o \
    wlcStdin.o \
    wlcUif.o \
    wlcWin.o \
    wlcWriteVer.o

base_wlc: $(foreach obj,$(base_wlc_targets),$(build_dir)/base/wlc/$(obj))

base_wln_targets := \
	wln.o \
    wlnBlast.o \
    wlnCom.o \
    wlnGuide.o \
    wlnMem.o \
    wlnNdr.o \
    wlnNtk.o \
    wlnObj.o \
    wlnRead.o \
    wlnRetime.o \
    wlnRtl.o \
    wlnWlc.o \
    wlnWriteVer.o

base_wln: $(foreach obj,$(base_wln_targets),$(build_dir)/base/wln/$(obj))

base_acb_targets := \
	acbAbc.o \
    acbAig.o \
    acbCom.o \
    acbFunc.o \
    acbMfs.o \
    acbPush.o \
    acbSets.o \
    acbTest.o \
    acbUtil.o 
	
base_acb: $(foreach obj,$(base_acb_targets),$(build_dir)/base/acb/$(obj))

base_bac_targets := \
	bacBlast.o \
    bacBac.o \
    bacCom.o \
    bacLib.o \
    bacNtk.o \
    bacPrsBuild.o \
    bacPrsTrans.o \
    bacPtr.o \
    bacPtrAbc.o \
    bacReadBlif.o \
    bacReadSmt.o \
    bacReadVer.o \
    bacWriteBlif.o \
    bacWriteSmt.o \
    bacWriteVer.o 

base_bac: $(foreach obj,$(base_bac_targets),$(build_dir)/base/bac/$(obj))

base_cba_targets := \
    cbaBlast.o \
    cbaCba.o \
    cbaCom.o \
    cbaNtk.o \
    cbaReadBlif.o \
    cbaReadVer.o \
    cbaWriteBlif.o \
    cbaWriteVer.o
	
base_cba: $(foreach obj,$(base_cba_targets),$(build_dir)/base/cba/$(obj))

base_pla_targets := \
    plaCom.o \
    plaHash.o \
    plaMan.o \
    plaMerge.o \
    plaSimple.o \
    plaRead.o \
    plaWrite.o

base_pla: $(foreach obj,$(base_pla_targets),$(build_dir)/base/pla/$(obj))

base: base_abc base_abci base_cmd base_io base_exor base_ver base_wlc base_wln base_acb base_bac base_cba base_pla

clean:
	rm -rf $(build_dir)

.PHONY: clean
