#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=MinGW_1-Windows
CND_DLIB_EXT=dll
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/Program_Control/MAHLERAN_1_2_3.o \
	${OBJECTDIR}/src/Program_Control/MAHLERAN_interstorm_setting_xml.o \
	${OBJECTDIR}/src/Program_Control/MAHLERAN_interstorm_xml.o \
	${OBJECTDIR}/src/Program_Control/MAHLERAN_storm_setting_xml.o \
	${OBJECTDIR}/src/Program_Control/MAHLERAN_storm_xml.o \
	${OBJECTDIR}/src/Program_Control/MAHLERAN_vegdyn_setting_xml.o \
	${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o \
	${OBJECTDIR}/src/Program_Control/parameters_from_xml.o \
	${OBJECTDIR}/src/Program_Control/shared_data.o \
	${OBJECTDIR}/src/Program_Control/vegdynamics_shared_data.o \
	${OBJECTDIR}/src/Rainfall_Converter/utils.o \
	${OBJECTDIR}/src/Subroutines_Chemistry/route_chemistry.o \
	${OBJECTDIR}/src/Subroutines_Chemistry/update_chemistry_flow.o \
	${OBJECTDIR}/src/Subroutines_In_out/Functions.o \
	${OBJECTDIR}/src/Subroutines_In_out/Set_rain_new_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/Set_rain_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/calculate_surface_properties_from_types.o \
	${OBJECTDIR}/src/Subroutines_In_out/calibration_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/contrib_area.o \
	${OBJECTDIR}/src/Subroutines_In_out/dynamic_topog_attribute.o \
	${OBJECTDIR}/src/Subroutines_In_out/echo_params_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/find_output_file_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/initialize_allocatables.o \
	${OBJECTDIR}/src/Subroutines_In_out/initialize_markers\ _xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/initialize_values_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/marked_cell_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/mt95.o \
	${OBJECTDIR}/src/Subroutines_In_out/output_file_definition_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/output_hydro_data_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/output_maps_within_storm_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/output_maps_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/output_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/output_markers_xy_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/randgen.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_Phi_maps.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_cover_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_ff_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_ksat_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_parameters_from_xml_file.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_pavement_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_rain_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_rain_mask.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_sm_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_spatial_data.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_theta_sat_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/read_veg_map.o \
	${OBJECTDIR}/src/Subroutines_In_out/topog_attrib.o \
	${OBJECTDIR}/src/Subroutines_In_out/topog_attribute.o \
	${OBJECTDIR}/src/Subroutines_In_out/xml-fortran/src/xmlparse.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/Annual_statistics.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/calc_et.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/calc_inf.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/calc_sm.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/calcday.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/calcyear.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/interstorm_initialize_xml.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/output_interstorm_fluxes.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/output_interstorm_xml.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/read_interstorm_parameters_xml.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/read_rainfall_daily_xml.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/read_rainfall_minute_xml.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/read_shrub_cov_xml.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/read_temperature_daily_xml.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/theta_areal_average.o \
	${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o \
	${OBJECTDIR}/src/Subroutines_Sediment/bedload_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/conc_flow_transport.o \
	${OBJECTDIR}/src/Subroutines_Sediment/diffuse_flow_transport.o \
	${OBJECTDIR}/src/Subroutines_Sediment/diffuse_transport_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/flow_detachment.o \
	${OBJECTDIR}/src/Subroutines_Sediment/flow_distrib.o \
	${OBJECTDIR}/src/Subroutines_Sediment/raindrop_detachment.o \
	${OBJECTDIR}/src/Subroutines_Sediment/route_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/route_sediment_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/splash_detach_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/splash_transport.o \
	${OBJECTDIR}/src/Subroutines_Sediment/splash_transport_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/susp_trajectories_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/suspended_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/suspended_transport.o \
	${OBJECTDIR}/src/Subroutines_Sediment/trans_detach_markers_xml.o \
	${OBJECTDIR}/src/Subroutines_Sediment/update_sediment_flow.o \
	${OBJECTDIR}/src/Subroutines_Sediment/update_top_surface.o \
	${OBJECTDIR}/src/Subroutines_Vegetation/dispersal.o \
	${OBJECTDIR}/src/Subroutines_Vegetation/growth.o \
	${OBJECTDIR}/src/Subroutines_Vegetation/mortality.o \
	${OBJECTDIR}/src/Subroutines_Vegetation/output_vegdyn.o \
	${OBJECTDIR}/src/Subroutines_Vegetation/veg_dyn.o \
	${OBJECTDIR}/src/Subroutines_Water/accumulate_flow.o \
	${OBJECTDIR}/src/Subroutines_Water/downslope_vars.o \
	${OBJECTDIR}/src/Subroutines_Water/infilt.o \
	${OBJECTDIR}/src/Subroutines_Water/route_water.o \
	${OBJECTDIR}/src/Subroutines_Water/update_water_flow.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/mahleran_v1.2.3.exe

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/mahleran_v1.2.3.exe: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.f} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/mahleran_v1.2.3 ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/Program_Control/MAHLERAN_1_2_3.o: src/Program_Control/MAHLERAN_1_2_3.f90 ${OBJECTDIR}/src/Program_Control/shared_data.o ${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o ${OBJECTDIR}/src/Program_Control/parameters_from_xml.o ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/MAHLERAN_1_2_3.o src/Program_Control/MAHLERAN_1_2_3.f90

${OBJECTDIR}/src/Program_Control/MAHLERAN_interstorm_setting_xml.o: src/Program_Control/MAHLERAN_interstorm_setting_xml.f90 ${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o ${OBJECTDIR}/src/Program_Control/parameters_from_xml.o
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/MAHLERAN_interstorm_setting_xml.o src/Program_Control/MAHLERAN_interstorm_setting_xml.f90

${OBJECTDIR}/src/Program_Control/MAHLERAN_interstorm_xml.o: src/Program_Control/MAHLERAN_interstorm_xml.f90 ${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o ${OBJECTDIR}/src/Program_Control/shared_data.o ${OBJECTDIR}/src/Program_Control/parameters_from_xml.o ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o ${OBJECTDIR}/src/Program_Control/vegdynamics_shared_data.o
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/MAHLERAN_interstorm_xml.o src/Program_Control/MAHLERAN_interstorm_xml.f90

${OBJECTDIR}/src/Program_Control/MAHLERAN_storm_setting_xml.o: src/Program_Control/MAHLERAN_storm_setting_xml.f90 ${OBJECTDIR}/src/Program_Control/shared_data.o ${OBJECTDIR}/src/Program_Control/parameters_from_xml.o
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/MAHLERAN_storm_setting_xml.o src/Program_Control/MAHLERAN_storm_setting_xml.f90

${OBJECTDIR}/src/Program_Control/MAHLERAN_storm_xml.o: src/Program_Control/MAHLERAN_storm_xml.f90 ${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o ${OBJECTDIR}/src/Program_Control/shared_data.o ${OBJECTDIR}/src/Program_Control/parameters_from_xml.o ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o 
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/MAHLERAN_storm_xml.o src/Program_Control/MAHLERAN_storm_xml.f90

${OBJECTDIR}/src/Program_Control/MAHLERAN_vegdyn_setting_xml.o: src/Program_Control/MAHLERAN_vegdyn_setting_xml.f90 ${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o ${OBJECTDIR}/src/Program_Control/vegdynamics_shared_data.o
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/MAHLERAN_vegdyn_setting_xml.o src/Program_Control/MAHLERAN_vegdyn_setting_xml.f90

${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o: src/Program_Control/interstorm_shared_data.f90
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/interstorm_shared_data.o src/Program_Control/interstorm_shared_data.f90

${OBJECTDIR}/src/Program_Control/parameters_from_xml.o: src/Program_Control/parameters_from_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/parameters_from_xml.o src/Program_Control/parameters_from_xml.f90

${OBJECTDIR}/src/Program_Control/shared_data.o: src/Program_Control/shared_data.f90
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/shared_data.o src/Program_Control/shared_data.f90

${OBJECTDIR}/src/Program_Control/vegdynamics_shared_data.o: src/Program_Control/vegdynamics_shared_data.f90
	${MKDIR} -p ${OBJECTDIR}/src/Program_Control
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Program_Control/vegdynamics_shared_data.o src/Program_Control/vegdynamics_shared_data.f90

${OBJECTDIR}/src/Rainfall_Converter/utils.o: src/Rainfall_Converter/utils.f90
	${MKDIR} -p ${OBJECTDIR}/src/Rainfall_Converter
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Rainfall_Converter/utils.o src/Rainfall_Converter/utils.f90

${OBJECTDIR}/src/Subroutines_Chemistry/route_chemistry.o: src/Subroutines_Chemistry/route_chemistry.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Chemistry
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Chemistry/route_chemistry.o src/Subroutines_Chemistry/route_chemistry.for

${OBJECTDIR}/src/Subroutines_Chemistry/update_chemistry_flow.o: src/Subroutines_Chemistry/update_chemistry_flow.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Chemistry
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Chemistry/update_chemistry_flow.o src/Subroutines_Chemistry/update_chemistry_flow.for

${OBJECTDIR}/src/Subroutines_In_out/Functions.o: src/Subroutines_In_out/Functions.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/Functions.o src/Subroutines_In_out/Functions.for

${OBJECTDIR}/src/Subroutines_In_out/Set_rain_new_xml.o: src/Subroutines_In_out/Set_rain_new_xml.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/Set_rain_new_xml.o src/Subroutines_In_out/Set_rain_new_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/Set_rain_xml.o: src/Subroutines_In_out/Set_rain_xml.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/Set_rain_xml.o src/Subroutines_In_out/Set_rain_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/calculate_surface_properties_from_types.o: src/Subroutines_In_out/calculate_surface_properties_from_types.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/calculate_surface_properties_from_types.o src/Subroutines_In_out/calculate_surface_properties_from_types.f90

${OBJECTDIR}/src/Subroutines_In_out/calibration_xml.o: src/Subroutines_In_out/calibration_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/calibration_xml.o src/Subroutines_In_out/calibration_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/contrib_area.o: src/Subroutines_In_out/contrib_area.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/contrib_area.o src/Subroutines_In_out/contrib_area.for

${OBJECTDIR}/src/Subroutines_In_out/dynamic_topog_attribute.o: src/Subroutines_In_out/dynamic_topog_attribute.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/dynamic_topog_attribute.o src/Subroutines_In_out/dynamic_topog_attribute.for

${OBJECTDIR}/src/Subroutines_In_out/echo_params_xml.o: src/Subroutines_In_out/echo_params_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/echo_params_xml.o src/Subroutines_In_out/echo_params_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/find_output_file_xml.o: src/Subroutines_In_out/find_output_file_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/find_output_file_xml.o src/Subroutines_In_out/find_output_file_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/initialize_allocatables.o: src/Subroutines_In_out/initialize_allocatables.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/initialize_allocatables.o src/Subroutines_In_out/initialize_allocatables.f90

.NO_PARALLEL:${OBJECTDIR}/src/Subroutines_In_out/initialize_markers\ _xml.o
${OBJECTDIR}/src/Subroutines_In_out/initialize_markers\ _xml.o: src/Subroutines_In_out/initialize_markers\ _xml.f90 ${OBJECTDIR}/src/Subroutines_In_out/mt95.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/initialize_markers\ _xml.o src/Subroutines_In_out/initialize_markers\ _xml.f90

${OBJECTDIR}/src/Subroutines_In_out/initialize_values_xml.o: src/Subroutines_In_out/initialize_values_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/initialize_values_xml.o src/Subroutines_In_out/initialize_values_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/marked_cell_xml.o: src/Subroutines_In_out/marked_cell_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/marked_cell_xml.o src/Subroutines_In_out/marked_cell_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/mt95.o: src/Subroutines_In_out/mt95.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/mt95.o src/Subroutines_In_out/mt95.f90

${OBJECTDIR}/src/Subroutines_In_out/output_file_definition_xml.o: src/Subroutines_In_out/output_file_definition_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/output_file_definition_xml.o src/Subroutines_In_out/output_file_definition_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/output_hydro_data_xml.o: src/Subroutines_In_out/output_hydro_data_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/output_hydro_data_xml.o src/Subroutines_In_out/output_hydro_data_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/output_maps_within_storm_xml.o: src/Subroutines_In_out/output_maps_within_storm_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/output_maps_within_storm_xml.o src/Subroutines_In_out/output_maps_within_storm_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/output_maps_xml.o: src/Subroutines_In_out/output_maps_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/output_maps_xml.o src/Subroutines_In_out/output_maps_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/output_markers_xml.o: src/Subroutines_In_out/output_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/output_markers_xml.o src/Subroutines_In_out/output_markers_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/output_markers_xy_xml.o: src/Subroutines_In_out/output_markers_xy_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/output_markers_xy_xml.o src/Subroutines_In_out/output_markers_xy_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/randgen.o: src/Subroutines_In_out/randgen.f
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/randgen.o src/Subroutines_In_out/randgen.f

${OBJECTDIR}/src/Subroutines_In_out/read_Phi_maps.o: src/Subroutines_In_out/read_Phi_maps.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_Phi_maps.o src/Subroutines_In_out/read_Phi_maps.for

${OBJECTDIR}/src/Subroutines_In_out/read_cover_map.o: src/Subroutines_In_out/read_cover_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_cover_map.o src/Subroutines_In_out/read_cover_map.for

${OBJECTDIR}/src/Subroutines_In_out/read_ff_map.o: src/Subroutines_In_out/read_ff_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_ff_map.o src/Subroutines_In_out/read_ff_map.for

${OBJECTDIR}/src/Subroutines_In_out/read_ksat_map.o: src/Subroutines_In_out/read_ksat_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_ksat_map.o src/Subroutines_In_out/read_ksat_map.for

${OBJECTDIR}/src/Subroutines_In_out/read_markers_xml.o: src/Subroutines_In_out/read_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_markers_xml.o src/Subroutines_In_out/read_markers_xml.f90

${OBJECTDIR}/src/Subroutines_In_out/read_parameters_from_xml_file.o: src/Subroutines_In_out/read_parameters_from_xml_file.f90 ${OBJECTDIR}/src/Subroutines_In_out/xml-fortran/src/xmlparse.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_parameters_from_xml_file.o src/Subroutines_In_out/read_parameters_from_xml_file.f90

${OBJECTDIR}/src/Subroutines_In_out/read_pavement_map.o: src/Subroutines_In_out/read_pavement_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_pavement_map.o src/Subroutines_In_out/read_pavement_map.for

${OBJECTDIR}/src/Subroutines_In_out/read_rain_map.o: src/Subroutines_In_out/read_rain_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_rain_map.o src/Subroutines_In_out/read_rain_map.for

${OBJECTDIR}/src/Subroutines_In_out/read_rain_mask.o: src/Subroutines_In_out/read_rain_mask.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_rain_mask.o src/Subroutines_In_out/read_rain_mask.for

${OBJECTDIR}/src/Subroutines_In_out/read_sm_map.o: src/Subroutines_In_out/read_sm_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_sm_map.o src/Subroutines_In_out/read_sm_map.for

${OBJECTDIR}/src/Subroutines_In_out/read_spatial_data.o: src/Subroutines_In_out/read_spatial_data.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_spatial_data.o src/Subroutines_In_out/read_spatial_data.f90

${OBJECTDIR}/src/Subroutines_In_out/read_theta_sat_map.o: src/Subroutines_In_out/read_theta_sat_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_theta_sat_map.o src/Subroutines_In_out/read_theta_sat_map.for

${OBJECTDIR}/src/Subroutines_In_out/read_veg_map.o: src/Subroutines_In_out/read_veg_map.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/read_veg_map.o src/Subroutines_In_out/read_veg_map.for

${OBJECTDIR}/src/Subroutines_In_out/topog_attrib.o: src/Subroutines_In_out/topog_attrib.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/topog_attrib.o src/Subroutines_In_out/topog_attrib.for

${OBJECTDIR}/src/Subroutines_In_out/topog_attribute.o: src/Subroutines_In_out/topog_attribute.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/topog_attribute.o src/Subroutines_In_out/topog_attribute.for

${OBJECTDIR}/src/Subroutines_In_out/xml-fortran/src/xmlparse.o: src/Subroutines_In_out/xml-fortran/src/xmlparse.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_In_out/xml-fortran/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_In_out/xml-fortran/src/xmlparse.o src/Subroutines_In_out/xml-fortran/src/xmlparse.f90

${OBJECTDIR}/src/Subroutines_Interstorm/Annual_statistics.o: src/Subroutines_Interstorm/Annual_statistics.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/Annual_statistics.o src/Subroutines_Interstorm/Annual_statistics.f90

${OBJECTDIR}/src/Subroutines_Interstorm/calc_et.o: src/Subroutines_Interstorm/calc_et.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/calc_et.o src/Subroutines_Interstorm/calc_et.f90

${OBJECTDIR}/src/Subroutines_Interstorm/calc_inf.o: src/Subroutines_Interstorm/calc_inf.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/calc_inf.o src/Subroutines_Interstorm/calc_inf.f90

${OBJECTDIR}/src/Subroutines_Interstorm/calc_sm.o: src/Subroutines_Interstorm/calc_sm.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/calc_sm.o src/Subroutines_Interstorm/calc_sm.f90

${OBJECTDIR}/src/Subroutines_Interstorm/calcday.o: src/Subroutines_Interstorm/calcday.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/calcday.o src/Subroutines_Interstorm/calcday.f90

${OBJECTDIR}/src/Subroutines_Interstorm/calcyear.o: src/Subroutines_Interstorm/calcyear.f90 ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/calcyear.o src/Subroutines_Interstorm/calcyear.f90

${OBJECTDIR}/src/Subroutines_Interstorm/interstorm_initialize_xml.o: src/Subroutines_Interstorm/interstorm_initialize_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/interstorm_initialize_xml.o src/Subroutines_Interstorm/interstorm_initialize_xml.f90

${OBJECTDIR}/src/Subroutines_Interstorm/output_interstorm_fluxes.o: src/Subroutines_Interstorm/output_interstorm_fluxes.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/output_interstorm_fluxes.o src/Subroutines_Interstorm/output_interstorm_fluxes.f90

${OBJECTDIR}/src/Subroutines_Interstorm/output_interstorm_xml.o: src/Subroutines_Interstorm/output_interstorm_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/output_interstorm_xml.o src/Subroutines_Interstorm/output_interstorm_xml.f90

${OBJECTDIR}/src/Subroutines_Interstorm/read_interstorm_parameters_xml.o: src/Subroutines_Interstorm/read_interstorm_parameters_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/read_interstorm_parameters_xml.o src/Subroutines_Interstorm/read_interstorm_parameters_xml.f90

${OBJECTDIR}/src/Subroutines_Interstorm/read_rainfall_daily_xml.o: src/Subroutines_Interstorm/read_rainfall_daily_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/read_rainfall_daily_xml.o src/Subroutines_Interstorm/read_rainfall_daily_xml.f90

${OBJECTDIR}/src/Subroutines_Interstorm/read_rainfall_minute_xml.o: src/Subroutines_Interstorm/read_rainfall_minute_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/read_rainfall_minute_xml.o src/Subroutines_Interstorm/read_rainfall_minute_xml.f90

${OBJECTDIR}/src/Subroutines_Interstorm/read_shrub_cov_xml.o: src/Subroutines_Interstorm/read_shrub_cov_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/read_shrub_cov_xml.o src/Subroutines_Interstorm/read_shrub_cov_xml.f90

${OBJECTDIR}/src/Subroutines_Interstorm/read_temperature_daily_xml.o: src/Subroutines_Interstorm/read_temperature_daily_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/read_temperature_daily_xml.o src/Subroutines_Interstorm/read_temperature_daily_xml.f90

${OBJECTDIR}/src/Subroutines_Interstorm/theta_areal_average.o: src/Subroutines_Interstorm/theta_areal_average.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/theta_areal_average.o src/Subroutines_Interstorm/theta_areal_average.f90

${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o: src/Subroutines_Interstorm/time_h.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Interstorm
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Interstorm/time_h.o src/Subroutines_Interstorm/time_h.f90

${OBJECTDIR}/src/Subroutines_Sediment/bedload_markers_xml.o: src/Subroutines_Sediment/bedload_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/bedload_markers_xml.o src/Subroutines_Sediment/bedload_markers_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/conc_flow_transport.o: src/Subroutines_Sediment/conc_flow_transport.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/conc_flow_transport.o src/Subroutines_Sediment/conc_flow_transport.for

${OBJECTDIR}/src/Subroutines_Sediment/diffuse_flow_transport.o: src/Subroutines_Sediment/diffuse_flow_transport.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/diffuse_flow_transport.o src/Subroutines_Sediment/diffuse_flow_transport.for

${OBJECTDIR}/src/Subroutines_Sediment/diffuse_transport_markers_xml.o: src/Subroutines_Sediment/diffuse_transport_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/diffuse_transport_markers_xml.o src/Subroutines_Sediment/diffuse_transport_markers_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/flow_detachment.o: src/Subroutines_Sediment/flow_detachment.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/flow_detachment.o src/Subroutines_Sediment/flow_detachment.for

${OBJECTDIR}/src/Subroutines_Sediment/flow_distrib.o: src/Subroutines_Sediment/flow_distrib.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/flow_distrib.o src/Subroutines_Sediment/flow_distrib.for

${OBJECTDIR}/src/Subroutines_Sediment/raindrop_detachment.o: src/Subroutines_Sediment/raindrop_detachment.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/raindrop_detachment.o src/Subroutines_Sediment/raindrop_detachment.for

${OBJECTDIR}/src/Subroutines_Sediment/route_markers_xml.o: src/Subroutines_Sediment/route_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/route_markers_xml.o src/Subroutines_Sediment/route_markers_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/route_sediment_xml.o: src/Subroutines_Sediment/route_sediment_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/route_sediment_xml.o src/Subroutines_Sediment/route_sediment_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/splash_detach_markers_xml.o: src/Subroutines_Sediment/splash_detach_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/splash_detach_markers_xml.o src/Subroutines_Sediment/splash_detach_markers_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/splash_transport.o: src/Subroutines_Sediment/splash_transport.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/splash_transport.o src/Subroutines_Sediment/splash_transport.for

${OBJECTDIR}/src/Subroutines_Sediment/splash_transport_markers_xml.o: src/Subroutines_Sediment/splash_transport_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/splash_transport_markers_xml.o src/Subroutines_Sediment/splash_transport_markers_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/susp_trajectories_xml.o: src/Subroutines_Sediment/susp_trajectories_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/susp_trajectories_xml.o src/Subroutines_Sediment/susp_trajectories_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/suspended_markers_xml.o: src/Subroutines_Sediment/suspended_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/suspended_markers_xml.o src/Subroutines_Sediment/suspended_markers_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/suspended_transport.o: src/Subroutines_Sediment/suspended_transport.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/suspended_transport.o src/Subroutines_Sediment/suspended_transport.for

${OBJECTDIR}/src/Subroutines_Sediment/trans_detach_markers_xml.o: src/Subroutines_Sediment/trans_detach_markers_xml.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/trans_detach_markers_xml.o src/Subroutines_Sediment/trans_detach_markers_xml.f90

${OBJECTDIR}/src/Subroutines_Sediment/update_sediment_flow.o: src/Subroutines_Sediment/update_sediment_flow.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/update_sediment_flow.o src/Subroutines_Sediment/update_sediment_flow.for

${OBJECTDIR}/src/Subroutines_Sediment/update_top_surface.o: src/Subroutines_Sediment/update_top_surface.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Sediment
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Sediment/update_top_surface.o src/Subroutines_Sediment/update_top_surface.for

${OBJECTDIR}/src/Subroutines_Vegetation/dispersal.o: src/Subroutines_Vegetation/dispersal.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Vegetation
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Vegetation/dispersal.o src/Subroutines_Vegetation/dispersal.f90

${OBJECTDIR}/src/Subroutines_Vegetation/growth.o: src/Subroutines_Vegetation/growth.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Vegetation
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Vegetation/growth.o src/Subroutines_Vegetation/growth.f90

${OBJECTDIR}/src/Subroutines_Vegetation/mortality.o: src/Subroutines_Vegetation/mortality.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Vegetation
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Vegetation/mortality.o src/Subroutines_Vegetation/mortality.f90

${OBJECTDIR}/src/Subroutines_Vegetation/output_vegdyn.o: src/Subroutines_Vegetation/output_vegdyn.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Vegetation
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Vegetation/output_vegdyn.o src/Subroutines_Vegetation/output_vegdyn.f90

${OBJECTDIR}/src/Subroutines_Vegetation/veg_dyn.o: src/Subroutines_Vegetation/veg_dyn.f90
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Vegetation
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Vegetation/veg_dyn.o src/Subroutines_Vegetation/veg_dyn.f90

${OBJECTDIR}/src/Subroutines_Water/accumulate_flow.o: src/Subroutines_Water/accumulate_flow.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Water
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Water/accumulate_flow.o src/Subroutines_Water/accumulate_flow.for

${OBJECTDIR}/src/Subroutines_Water/downslope_vars.o: src/Subroutines_Water/downslope_vars.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Water
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Water/downslope_vars.o src/Subroutines_Water/downslope_vars.for

${OBJECTDIR}/src/Subroutines_Water/infilt.o: src/Subroutines_Water/infilt.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Water
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Water/infilt.o src/Subroutines_Water/infilt.for

${OBJECTDIR}/src/Subroutines_Water/route_water.o: src/Subroutines_Water/route_water.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Water
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Water/route_water.o src/Subroutines_Water/route_water.for

${OBJECTDIR}/src/Subroutines_Water/update_water_flow.o: src/Subroutines_Water/update_water_flow.for
	${MKDIR} -p ${OBJECTDIR}/src/Subroutines_Water
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/Subroutines_Water/update_water_flow.o src/Subroutines_Water/update_water_flow.for

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} *.mod

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
