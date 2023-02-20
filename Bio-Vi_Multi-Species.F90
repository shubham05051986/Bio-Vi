#include "fabm_driver.h"

!-------------------------------------------------------------------------------
! ------------------------------ Interface -------------------------------------

  module gotm_npzd
!-------------------------------------------------------------------------------
! ----------------------------- Description ------------------------------------

!-------------------------------------------------------------------------------
! -------------------------------- Uses ----------------------------------------

  use fabm_types

  implicit none

  private

!-------------------------------------------------------------------------------
! ------------------------- Public Derived Types -------------------------------

  type,extends(type_base_model),public :: type_gotm_npzd

!-------------------------------------------------------------------------------
! ------------------------- Variable Identifiers -------------------------------

    type (type_state_variable_id)         :: id_n, id_dia, id_pico, id_zs, id_zl, id_vd, id_vp, id_infd, id_infp, id_d
    type (type_state_variable_id)         :: id_dic
    type (type_dependency_id)             :: id_par, id_temp
    type (type_horizontal_dependency_id)  :: id_I_0
    type (type_diagnostic_variable_id)    :: id_GPP, id_NCP, id_PPR, id_NPR, id_dPAR

!-------------------------------------------------------------------------------
! --------------------------- Model Parameters ---------------------------------

    real (rk) :: p0, z0, kc, i_min, R, knd, knp, rpn
    real (rk) :: gP_d, gP_p, rpdu, rpdl, mPi, eZ, r_zs, r_zl, mZ, wZ, mV, rV
    real (rk) :: vD, vP, rD, nV, nD, kB, n, p_ads, p_inf
    real (rk) :: p_capd, p_capp, Bmin, Bmax, Topt_d, T1_d, T2_d
    real (rk) :: Topt_p, T1_p, T2_p, w_di, w_p, w_d, dic_per_n
  contains
    procedure :: initialize
    procedure :: do
    procedure :: do_ppdd
    procedure :: get_light_extinction
  end type

!-------------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------------
! ------------------------------ Interface -------------------------------------

  subroutine initialize(self,configunit)

!-------------------------------------------------------------------------------
! --------------------------- Input Parameters ---------------------------------

  class (type_gotm_npzd), intent(inout), target :: self
  integer,                intent(in)            :: configunit

!-------------------------------------------------------------------------------
! ---------------------------- Local Variables ---------------------------------

  real(rk)          :: n_initial
  real(rk)          :: dia_initial
  real(rk)          :: pico_initial
  real(rk)          :: z_initial
  real(rk)          :: v_initial
  real(rk)          :: infd_initial
  real(rk)          :: infp_initial
  real(rk)          :: d_initial
  real(rk)          :: p0
  real(rk)          :: z0
  real(rk)          :: kc
  real(rk)          :: i_min
  real(rk)          :: R
  real(rk)          :: knd
  real(rk)          :: knp
  real(rk)          :: rpn
  real(rk)          :: gP_d
  real(rk)          :: gP_p
  real(rk)          :: rpdu
  real(rk)          :: rpdl
  real(rk)          :: mPi
  real(rk)          :: eZ
  real(rk)          :: r_zs
  real(rk)          :: r_zl
  real(rk)          :: mZ
  real(rk)          :: wZ
  real(rk)          :: mV
  real(rk)          :: rV
  real(rk)          :: vD
  real(rk)          :: vP
  real(rk)          :: rD
  real(rk)          :: nV
  real(rk)          :: nD
  real(rk)          :: kB
  real(rk)          :: n
  real(rk)          :: p_ads
  real(rk)          :: p_inf
  real(rk)          :: p_capd
  real(rk)          :: p_capp
  real(rk)          :: Bmin
  real(rk)          :: Bmax
  real(rk)          :: Topt_d
  real(rk)          :: T1_d
  real(rk)          :: T2_d
  real(rk)          :: Topt_p
  real(rk)          :: T1_p
  real(rk)          :: T2_p
  real(rk)          :: w_di
  real(rk)          :: w_p
  real(rk)          :: w_d
  real(rk)          :: dic_per_n
  character(len=64) :: dic_variable

  real(rk), parameter :: d_per_s = 1.0_rk/86400.0_rk
  namelist /gotm_npzd/ n_initial,dia_initial,pico_initial,z_initial,v_initial,         &
                       infd_initial,infp_initial,d_initial,p0,z0,kc,i_min,R,knd,knp,   &
                       rpn,gP_d,gP_p,rpdu,rpdl,mPi,eZ,r_zs,r_zl,mZ,wZ,mV,                &
                       rV,vD,vP,rD,nV,nD,kB,n,p_ads,p_inf,p_capd,p_capp,         &
                       Bmin,Bmax,Topt_d,T1_d,T2_d,Topt_p,T1_p,T2_p,w_di,w_p,w_d,dic_per_n

!-------------------------------------------------------------------------------

  n_initial      = 0.0_rk
  dia_initial    = 0.0_rk
  pico_initial   = 0.0_rk
  z_initial      = 0.0_rk
  v_initial      = 0.0_rk
  infd_initial   = 0.0_rk
  infp_initial   = 0.0_rk
  d_initial      = 0.0_rk
  p0             = 0.0_rk
  z0             = 0.0_rk
  kc             = 0.0_rk
  i_min          = 0.0_rk
  R              = 0.0_rk
  knd            = 0.0_rk
  knp            = 0.0_rk
  rpn            = 0.0_rk
  gP_d           = 0.0_rk
  gP_p           = 0.0_rk
  rpdu           = 0.0_rk
  rpdl           = 0.0_rk
  mPi            = 0.0_rk
  eZ             = 0.0_rk
  r_zs           = 0.0_rk
  r_zl           = 0.0_rk
  mZ             = 0.0_rk
  wZ             = 0.0_rk
  mV             = 0.0_rk
  rV             = 0.0_rk
  vD             = 0.0_rk
  vP             = 0.0_rk
  rD             = 0.0_rk
  nV             = 0.0_rk
  nD             = 0.0_rk
  kB             = 0.0_rk
  n              = 0.0_rk
  p_ads          = 0.0_rk
  p_inf          = 0.0_rk
  p_capd         = 0.0_rk
  p_capp         = 0.0_rk
  Bmin           = 0.0_rk
  Bmax           = 0.0_rk
  Topt_d         = 0.0_rk
  T1_d           = 0.0_rk
  T2_d           = 0.0_rk
  Topt_p         = 0.0_rk
  T1_p           = 0.0_rk
  T2_p           = 0.0_rk
  w_di           = 0.0_rk
  w_p            = 0.0_rk
  w_d            = 0.0_rk
  dic_per_n      = 0.0_rk
  dic_variable   = ''

! ------------------------------------------------------------------------------
! -------------------------- Read the namelist ---------------------------------

  if (configunit>0) read(configunit,nml=gotm_npzd,err=99,end=100)

! ------------------------------------------------------------------------------
! ---------------- Store parameters in our own derived type --------------------
! ------- rates in values per day and converted to values per second -----------

  call self%get_parameter(self%p0,       'p0',       'mmol m-3',    'background phytoplankton concentration ',default=p0)
  call self%get_parameter(self%z0,       'z0',       'mmol m-3',    'background zooplankton concentration',default=z0)
  call self%get_parameter(self%kc,       'kc',       'm2 mmol-1',   'specific light extinction of phytoplankton and detritus',default=kc)
  call self%get_parameter(self%i_min,    'i_min',    'W m-2',       'minimum light intensity in euphotic zone',default=i_min)
  call self%get_parameter(self%R,        'R',        'd-1',         'remineralization rate of detritus',default=R,scale_factor=d_per_s)
  call self%get_parameter(self%knd,      'knd',      'mmol m-3',    'half saturation constant diatoms',default=knd)
  call self%get_parameter(self%knp,      'knp',      'mmol m-3',    'half saturation constant pico',default=knp)
  call self%get_parameter(self%gP_d,     'gP_d',     'd-1',         'growth rate diatoms',default=gP_d,scale_factor=d_per_s)
  call self%get_parameter(self%gP_p,     'gP_p',     'd-1',         'growth rate picophytoplankton',default=gP_p,scale_factor=d_per_s)
  call self%get_parameter(self%rpdu,     'rpdu',     'd-1',         'phytoplankton mortality in euphotic zone',default=rpdu,scale_factor=d_per_s)
  call self%get_parameter(self%rpdl,     'rpdl',     'd-1',         'phytoplankton mortality below euphotic zone',default=rpdl,scale_factor=d_per_s)
  call self%get_parameter(self%mPi,      'mPi',      'd-1',         'lysis rate of infected phytoplankton',default=mPi,scale_factor=d_per_s)
  call self%get_parameter(self%eZ,       'eZ',       '',            'efficiency rate zooplankton grazing',default=eZ)
  call self%get_parameter(self%r_zs,     'r_zs',     'm',           'radius of small zooplankton',default=r_zs)
  call self%get_parameter(self%r_zl,     'r_zl',     'm',           'radius of large zooplankton',default=r_zl)
  call self%get_parameter(self%mZ,       'mZ',       'd-1',         'zooplankton mortality',default=mZ,scale_factor=d_per_s)
  call self%get_parameter(self%wZ,       'wZ',       'd-1',         'loss rate of zooplankton to nutrients',default=wZ,scale_factor=d_per_s)
  call self%get_parameter(self%mV,       'mV',       'd-1',         'decay of viruses',default=mV,scale_factor=d_per_s)
  call self%get_parameter(self%rV,       'rV',       'm',           'radius of viruses particle',default=rV)
  call self%get_parameter(self%vD,       'vD',       'µm^3',        'volume of diatom cell at 15°',default=vD)
  call self%get_parameter(self%vP,       'vP',       'µm^3',        'volume of picophytoplankton cell at 15°',default=vP)
  call self%get_parameter(self%rD,       'rD',       'm',           'radius of detritus particles',default=rD)
  call self%get_parameter(self%nV,       'nV',       'mmol N',      'N content of viruses particle',default=nV)
  call self%get_parameter(self%nD,       'nD',       'mmol N',      'N content of detritus particles',default=nD)
  call self%get_parameter(self%kB,       'kB',       'g m2 s-2 K-1','Boltzman Constant',default=kB)
  call self%get_parameter(self%n,        'n',        'g m2 s-1',    'dynamic viscosity',default=n)
  call self%get_parameter(self%p_ads,    'p_ads',    '-',           'probability of adhesion',default=p_ads)
  call self%get_parameter(self%p_inf,    'p_inf',    '-',           'probability of infection',default=p_inf)
  call self%get_parameter(self%p_capd,   'p_capd',   '-',           'probability of capture dia',default=p_capd)
  call self%get_parameter(self%p_capp,   'p_capp',   '-',           'probability of capture pico',default=p_capp)
  call self%get_parameter(self%Bmin,     'Bmin',     'particles',   'minimum burst size',default=Bmin)
  call self%get_parameter(self%Bmax,     'Bmax',     'particles',   'maximum burst size',default=Bmax)
  call self%get_parameter(self%Topt_d,   'Topt_d',   '°C',          'temperature optimum for diatom growth',default=Topt_d)
  call self%get_parameter(self%T1_d,     'T1_d',     '°C',          'width parameter of reaction norm dia',default=T1_d)
  call self%get_parameter(self%T2_d,     'T2_d',     '°C',          'width parameter of reaction norm dia',default=T2_d)
  call self%get_parameter(self%Topt_p,   'Topt_p',   '°C',          'temperature optimum for picophytoplankton growth',default=Topt_p)
  call self%get_parameter(self%T1_p,     'T1_p',     '°C',          'width parameter of reaction norm pico',default=T1_p)
  call self%get_parameter(self%T2_p,     'T2_p',     '°C',          'width parameter of reaction norm pico',default=T2_p)
  call self%get_parameter(w_di,          'w_di',     'm d-1',       'vertical velocity of diatoms (<0 for sinking)',default=w_di, scale_factor=d_per_s)
  call self%get_parameter(w_p,           'w_p',      'm d-1',       'vertical velocity of picophytoplankton (<0 for sinking)',default=w_p, scale_factor=d_per_s)
  call self%get_parameter(w_d,           'w_d',      'm d-1',       'vertical velocity of detritus  (<0 for sinking)',default=w_d,scale_factor=d_per_s)
  call self%get_parameter(self%dic_per_n,'dic_per_n','-',           'C:N ratio of biomass',default=dic_per_n)

! ------------------------------------------------------------------------------
! ------------------------ Register State Variables ----------------------------

  call self%register_state_variable(self%id_n,      'nut',        'mmol m-3','nutrients',                 n_initial,      minimum=0.0_rk, no_river_dilution=.true.)
  call self%register_state_variable(self%id_dia,    'dia',        'mmol m-3','phytoplankton',             dia_initial,    minimum=0.0_rk, vertical_movement=w_di)
  call self%register_state_variable(self%id_pico,   'pico',       'mmol m-3','phytoplankton',             pico_initial,   minimum=0.0_rk, vertical_movement=w_p)
  call self%register_state_variable(self%id_zs,     's_zoo',      'mmol m-3','small zooplankton',         z_initial,      minimum=0.0_rk)
  call self%register_state_variable(self%id_zl,     'l_zoo',      'mmol m-3','large zooplankton',         z_initial,      minimum=0.0_rk)
  call self%register_state_variable(self%id_d,      'det',        'mmol m-3','detritus',                  d_initial,      minimum=0.0_rk, vertical_movement=w_d)
  call self%register_state_variable(self%id_vd,     'd_vir',      'mmol m-3','diatom viruses',            v_initial,      minimum=0.0_rk)
  call self%register_state_variable(self%id_vp,     'p_vir',      'mmol m-3','pico viruses',              v_initial,      minimum=0.0_rk)
  call self%register_state_variable(self%id_infd,   'inf_dia',    'mmol m-3','infected diatoms',          infd_initial,   minimum=0.0_rk, vertical_movement=w_di)
  call self%register_state_variable(self%id_infp,   'inf_pico',   'mmol m-3','infected picophytoplankton',infp_initial,   minimum=0.0_rk, vertical_movement=w_p)

! ------------------------------------------------------------------------------
! ----------------- Register contribution to total Nitrogen --------------------

  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_n)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_dia)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_pico)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_zs)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_zl)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_d)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_vd)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_vp)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_infd)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_infp)

! ------------------------------------------------------------------------------
! -------------------- Register link to external DIC pool ----------------------
  call self%register_state_dependency(self%id_dic,'dic','mmol m-3','total dissolved inorganic carbon',required=.false.)
  if (dic_variable/='') call self%request_coupling(self%id_dic,dic_variable)


! ------------------------------------------------------------------------------
! ---------------------- Register diagnostic variables -------------------------

  call self%register_diagnostic_variable(self%id_GPP,'GPP','mmol m-3',  'gross primary production',           &
                    output=output_time_step_integrated)
  call self%register_diagnostic_variable(self%id_NCP,'NCP','mmol m-3',  'net community production',           &
                    output=output_time_step_integrated)
  call self%register_diagnostic_variable(self%id_PPR,'PPR','mmol m-3 d-1','gross primary production rate',      &
                    output=output_time_step_averaged)
  call self%register_diagnostic_variable(self%id_NPR,'NPR','mmol m-3 d-1','net community production rate',      &
                    output=output_time_step_averaged)
  call self%register_diagnostic_variable(self%id_dPAR,'PAR','W m-2',    'photosynthetically active radiation',&
                    output=output_time_step_averaged)

! ------------------------------------------------------------------------------
! ------------------- Register environmental dependencies ----------------------

  call self%register_dependency(self%id_par, standard_variables%downwelling_photosynthetic_radiative_flux)
  call self%register_dependency(self%id_I_0, standard_variables%surface_downwelling_photosynthetic_radiative_flux)
  call self%register_dependency(self%id_temp, standard_variables%temperature)

  return

99 call self%fatal_error('gotm_npzd_init','Error reading namelist gotm_npzd.')

100 call self%fatal_error('gotm_npzd_init','Namelist gotm_npzd was not found.')

  end subroutine initialize
! ------------------------------------------------------------------------------
! ------------------- Right hand sides of NPZD-V model -------------------------
! ------------------------------ Interface -------------------------------------

  subroutine do(self,_ARGUMENTS_DO_)

! ------------------------------------------------------------------------------
! --------------------------- Input Parameters ---------------------------------

  class (type_gotm_npzd),intent(in) :: self
  _DECLARE_ARGUMENTS_DO_

! ------------------------------------------------------------------------------
! ------------------------------- Local Variables ------------------------------

  real(rk)                   :: n, dia, pico, zs, zl, d, vd, vp, infd, infp, par, I_0, temp
  real(rk)                   :: temp_K, rpd, vol_d, r_d, n_d, vol_p, r_p, n_p
  real(rk)                   :: n_zs, n_zl, d_v, d_di, d_p, upreds, upredl, d_zs, d_zl,d_d
  real(rk)                   :: c_vdi, c_vp, c_vd, c_zd, c_zp, g_d, g_p, g_infd, g_infp
  real(rk)                   :: inf_d, inf_p, ads_vd, ads_vp, a_t, b_d, b_p, i_limp, n_limd, t_limd
  real(rk)                   :: n_limp, t_limp, b_limd, b_limp, prop_vd, prop_vp, i_decay_vd, i_decay_vp
  real(rk)                   :: pp_d, pp_p, dn
  real(rk), parameter        :: secs_pr_day = 86400.

! ------------------------------------------------------------------------------
! --------------------------------- Begin Loop ---------------------------------

  _LOOP_BEGIN_

! ------------------------------------------------------------------------------
! --------------- Retrieve current (local) state variable values ---------------

  _GET_(self%id_n,    n)             ! nutrient
  _GET_(self%id_dia,  dia)           ! diatoms
  _GET_(self%id_pico, pico)          ! picophytoplankton
  _GET_(self%id_zs,   zs)            ! small zooplankton
  _GET_(self%id_zl,   zl)            ! large zooplankton
  _GET_(self%id_d,    d)             ! detritus
  _GET_(self%id_vd,   vd)            ! diatom viruses
  _GET_(self%id_vp,   vp)            ! pico viruses
  _GET_(self%id_infd, infd)          ! infected diatoms
  _GET_(self%id_infp, infp)          ! infected picophytoplankton

! ------------------------------------------------------------------------------
! ----------------- Retrieve current environmental conditions ------------------

  _GET_(self%id_par,par)             ! local photosynthetically active radiation
  _GET_HORIZONTAL_(self%id_I_0,I_0)  ! surface short wave radiation
  _GET_(self%id_temp,temp)           ! temperature in °C
  temp_K = temp+273.15               ! temperature in Kelvin

! ------------------------------------------------------------------------------
! -- Loss rate of phytoplankton to detritus depends on local light intensity ---

  if (par .ge. self%I_min) then
     rpd = self%rpdu
  else
     rpd = self%rpdl
  end if

! ------------------------------------------------------------------------------
! ------------------ Temperature size dependency calculations ------------------
! ---------------------------------- Diatoms -----------------------------------

  vol_d     = self%vD + self%vD * 0.025 * (15 - temp)               !volume calculation based on temp
  r_d       = (((vol_d * 3)/(4 * 3.14))**(1/3)) * 1e-6              !radius from volume
  n_d       = ((0.288 * (vol_d**0.811)) * 0.15e-6)/14               !N from volume

! ---------------------------- Picophytoplankton -------------------------------

  vol_p     = self%vP + self%vP * 0.025 * (15 - temp)               !volume calculation based on temp
  r_p       = (((vol_p * 3)/(4 * 3.14))**(1/3)) * 1e-6              !radius from volume
  n_p       = ((0.288 * (vol_p**0.811)) * 0.15e-6)/14               !N from volume


! ------------ Zooplankton size as function of phytoplankton size --------------

  n_zs       = (0.0546 + 0.0137 * ((4/3) * 3.14 * (self%r_zs * 1e3)**3)) * 1e3/14   !small zooplankton N content

  n_zl       = (0.0546 + 0.0137 * ((4/3) * 3.14 * (self%r_zl * 1e3)**3)) * 1e3/14   !large zooplankton N content

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion equations -----------------------------
! ----------------------------------- Viruses ----------------------------------

  d_v       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rV)

! ----------------------------------- Diatoms ----------------------------------

  d_di      = (self%kB * temp_K)/(6 * 3.14 * self%n * r_d)

! ------------------------------ Picophytoplankton -----------------------------

  d_p       = (self%kB * temp_K)/(6 * 3.14 * self%n * r_p)

! --------------------------- Zooplankton (swimming)---------------------------

  upreds     = exp(0.4 + 0.8 * log(self%r_zs * 200)) * 0.01           !swimming small zooplankton

  upredl     = exp(0.4 + 0.8 * log(self%r_zl * 200)) * 0.01           !swimming large zooplankton

! ---------------------------------- Zooplankton -------------------------------

  d_zs       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%r_zs)     !diffusion of small zooplankton

  d_zl       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%r_zl)     !diffusion of large zooplankton

! ---------------------------------- Detritus ----------------------------------

  d_d       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rD)

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion based Contact -------------------------
! ------------------------------ Viruses and Diatoms ---------------------------

  c_vdi     = 4 * 3.14 * (r_d + self%rV) * (d_v + d_di) *86400

! ------------------------- Viruses and Picophytoplankton ----------------------

  c_vp      = 4 * 3.14 * (r_p + self%rV) * (d_v + d_p) *86400

! ------------------------------ Viruses and Detritus --------------------------

  c_vd      = 4 * 3.14 * (self%rD + self%rV) * (d_v + d_d) *86400

! --------------------------- Zooplankton and Diatoms --------------------------

  c_zd      = ((4 * 3.14 * (d_di + d_zl) * (r_d + self%r_zl)) + 3.14 * ((r_d + 3 * self%r_zl)**2) * upredl) *86400

! ---------------------- Zooplankton and Picophytoplankton ---------------------

  c_zp      = ((4 * 3.14 * (d_p + d_zs) * (r_p + self%r_zs)) + 3.14 * ((r_p + 3 * self%r_zs)**2) * upreds) *86400

! ------------------------------------------------------------------------------
! ------------------- Contact based Grazing, Infection, Decay ------------------
! ---------------------------------- Grazing -----------------------------------
! --------------------------- Zooplankton and Diatoms --------------------------

  g_d       = c_zd * (zl + self%z0) * (dia + self%p0) * (1/n_zl) * self%p_capd

! ---------------------- Zooplankton and Picophytoplankton ---------------------

  g_p       = c_zp * (zs + self%z0) * (pico + self%p0) * (1/n_zs) * self%p_capp

! ----------------------- Zooplankton and Infected Diatoms ---------------------

  g_infd    = c_zd * (zl + self%z0) * infd * (1/n_zl) * self%p_capd

! ------------------ Zooplankton and Infected Picophytoplankton ----------------

  g_infp    = c_zp * (zs + self%z0) * infp * (1/n_zs) * self%p_capp

! --------------------------------- Infection ----------------------------------
! ------------------------------ Viruses and Diatoms ---------------------------

  inf_d     = c_vdi * (dia + self%p0) * vd * (1/self%nV) * self%p_inf * self%p_ads

! ------------------------- Viruses and Picophytoplankton ----------------------

  inf_p     = c_vp * (pico + self%p0) * vp * (1/self%nV) * self%p_inf * self%p_ads

! ----------------------------------- Decay ------------------------------------
! ------------------------------ Viruses and Detritus --------------------------

  ads_vd    = c_vd * d * vd * (1/self%nD) * self%p_ads
  ads_vp    = c_vd * d * vp * (1/self%nD) * self%p_ads

! ------------------------------------------------------------------------------
! ---------------------------- Limitation Equations ----------------------------
  a_t       = 1
  b_d       = temp - self%Topt_d
  b_p       = temp - self%Topt_p
  i_limp    = 1 - exp(-0.07*par/1.0)

! ----------------------------------- Diatoms ----------------------------------

  n_limd    = n/(self%knd + n)
  t_limd    = exp(-((temp - self%Topt_d)**2)/((self%T1_d - self%T2_d * SIGN(a_t,b_d))**2))

! ------------------------------ Picophytoplankton -----------------------------

  n_limp    = n/(self%knp + n)
  t_limp    = exp(-((temp - self%Topt_p)**2)/((self%T1_p - self%T2_p * SIGN(a_t,b_p))**2))

! ----------------------------------- Viruses ----------------------------------

  b_limd     = i_limp * n_limd * t_limd                            !burst lim for diatoms as function of diatom growth limitations
  b_limp     = i_limp * n_limp * t_limp                            !burst lim for picophytoplankton as function of picophytoplankton growth limitations
  prop_vd    = (self%Bmin + self%Bmax * b_limd) * (self%nV / n_d)  !proportion of diatom cells converted to viral progeny in relation to burst size and N content
  prop_vp    = (self%Bmin + self%Bmax * b_limp) * (self%nV / n_p)  !proportion of picophytoplankton cells converted to viral progeny in relation to burst size and N content
  i_decay_vd = self%mV * (1 - exp(-0.1*par/3.0)) * vd              !decay of diatom viruses from radiation
  i_decay_vp = self%mV * (1 - exp(-0.1*par/3.0)) * vp              !decay of picophytoplankton viruses from radiation

! ------------------------------------------------------------------------------
! ----------------------------- Primary Production -----------------------------
! ----------------------------------- Diatoms ----------------------------------

  pp_d = self%gP_d * i_limp * n_limd * t_limd * (dia + self%p0)

! ------------------------------ Picophytoplankton -----------------------------

  pp_p = self%gP_p * i_limp * n_limp * t_limp * (pico + self%p0)

! ------------------------------------------------------------------------------
! ------------------------- Source/Sinks for Nutrients -------------------------

  dn = (self%rpn * dia) + (self%rpn * pico) + (self%wZ * zs) + (self%wZ * zl)+ (self%R * d) - pp_d - pp_p

! ------------------------------------------------------------------------------
! -------------------------- Set temporal derivatives --------------------------

  _SET_ODE_(self%id_n,        dn)
  _SET_ODE_(self%id_dia,      pp_d - g_d - (self%rpn * dia) - (rpd * dia) - inf_d)
  _SET_ODE_(self%id_pico,     pp_p - g_p - (self%rpn * pico) - (rpd * pico) - inf_p)
  _SET_ODE_(self%id_zs,       self%eZ * (g_p + g_infp) - (self%wZ * zs) - (self%mZ * (zs**2)) )
  _SET_ODE_(self%id_zl,       self%eZ * (g_d + g_infd) - (self%wZ * zl) - (self%mZ * (zl**2)) )
  _SET_ODE_(self%id_d,        rpd * dia + rpd * pico + self%mZ * (zs**2) + self%mZ * (zl**2) + (1-self%eZ) * (g_d + g_p + g_infd + g_infp) + (1-prop_vd)*self%mPi*infd + (1-prop_vp)*self%mPi*infp + ads_vd + ads_vp - self%R*d)
  _SET_ODE_(self%id_vd,       prop_vd * self%mPi * infd - i_decay_vd - ads_vd)
  _SET_ODE_(self%id_vp,       prop_vp * self%mPi * infp - i_decay_vp - ads_vp)
  _SET_ODE_(self%id_infd,     inf_d - self%mPi * infd - g_infd)
  _SET_ODE_(self%id_infp,     inf_p - self%mPi * infp - g_infp)

! ------------------------------------------------------------------------------
! ----------------- DIC pool change according to change in nut -----------------

  if (_AVAILABLE_(self%id_dic)) _SET_ODE_(self%id_dic,self%dic_per_n*dn)

! ------------------------------------------------------------------------------
! ------------------------- Export diagnostic variables ------------------------

  _SET_DIAGNOSTIC_(self%id_dPAR,  par)
  _SET_DIAGNOSTIC_(self%id_GPP ,  pp_d + pp_p)
  _SET_DIAGNOSTIC_(self%id_NCP ,  pp_d + pp_p - self%rpn * dia - self%rpn * pico)
  _SET_DIAGNOSTIC_(self%id_PPR ,  (pp_d + pp_p) * secs_pr_day)
  _SET_DIAGNOSTIC_(self%id_NPR ,  (pp_d + pp_p - self%rpn * dia - self%rpn * pico) * secs_pr_day)

! ------------------------------------------------------------------------------
! ---------------------------------- End Loop ----------------------------------

  _LOOP_END_

  end subroutine do

! ------------------------------------------------------------------------------
! ------------------------------ Light Extinction ------------------------------

   subroutine get_light_extinction(self,_ARGUMENTS_GET_EXTINCTION_)

   class (type_gotm_npzd), intent(in) :: self
   _DECLARE_ARGUMENTS_GET_EXTINCTION_

  real(rk)                            :: dia,pico,d

! ------------------------------------------------------------------------------
! -------------------------------- Begin Loop ----------------------------------

  _LOOP_BEGIN_

  _GET_(self%id_dia,  dia)   ! diatoms
  _GET_(self%id_pico, pico)  ! pico
  _GET_(self%id_d,    d)     ! detritus

  _SET_EXTINCTION_(self%kc * (self%p0 + dia + pico + d))

! ------------------------------------------------------------------------------
! ---------------------------------- End Loop ----------------------------------

  _LOOP_END_

  end subroutine get_light_extinction

! ------------------------------------------------------------------------------
! ---------------- Exporting Production/Destruction Matrices -------------------

  subroutine do_ppdd(self, _ARGUMENTS_DO_PPDD_)

  class (type_gotm_npzd), intent(in) :: self
  _DECLARE_ARGUMENTS_DO_PPDD_

! ------------------------------------------------------------------------------
! ------------------------------- Local Variables ------------------------------

  real(rk)                   :: n, dia, pico, zs, zl, d, vd, vp, infd, infp, par, I_0, temp
  real(rk)                   :: temp_K, rpd, vol_d, r_d, n_d, vol_p, r_p, n_p
  real(rk)                   :: n_zs, n_zl, d_v, d_di, d_p, upreds, upredl, d_zs, d_zl, d_d
  real(rk)                   :: c_vdi, c_vp, c_vd, c_zd, c_zp, g_d, g_p, g_infd, g_infp
  real(rk)                   :: inf_d, inf_p, ads_vd, ads_vp, a_t, b_d, b_p, i_limp, n_limd, t_limd
  real(rk)                   :: n_limp, t_limp, b_limd, b_limp, prop_vd, prop_vp, i_decay_vd, i_decay_vp
  real(rk)                   :: pp_d, pp_p, dn
  real(rk), parameter        :: secs_pr_day = 86400.

! ------------------------------------------------------------------------------
! --------------------------------- Begin Loop ---------------------------------

  _LOOP_BEGIN_

! ------------------------------------------------------------------------------
! --------------- Retrieve current (local) state variable values ---------------
  _GET_(self%id_n,    n)             ! nutrient
  _GET_(self%id_dia,  dia)           ! diatoms
  _GET_(self%id_pico, pico)          ! picophytoplankton
  _GET_(self%id_zs,   zs)            ! small zooplankton
  _GET_(self%id_zl,   zl)            ! large zooplankton
  _GET_(self%id_d,    d)             ! detritus
  _GET_(self%id_vd,   vd)            ! diatom viruses
  _GET_(self%id_vp,   vp)            ! pico viruses
  _GET_(self%id_infd, infd)          ! infected diatoms
  _GET_(self%id_infp, infp)          ! infected picophytoplankton

! ------------------------------------------------------------------------------
! ----------------- Retrieve current environmental conditions ------------------
  _GET_(self%id_par,par)             ! local photosynthetically active radiation
  _GET_HORIZONTAL_(self%id_I_0,I_0)  ! surface short wave radiation
  _GET_(self%id_temp,temp)           ! temperature in °C
  temp_K = temp+273.15               ! temperature in Kelvin

! ------------------------------------------------------------------------------
! -- Loss rate of phytoplankton to detritus depends on local light intensity ---

  if (par .ge. self%I_min) then
     rpd = self%rpdu
  else
     rpd = self%rpdl
  end if

! ------------------------------------------------------------------------------
! ------------------ Temperature size dependency calculations ------------------
! ---------------------------------- Diatoms -----------------------------------

  vol_d     = self%vD + self%vD * 0.025 * (15 - temp)               !volume calculation based on temp
  r_d       = (((vol_d * 3)/(4 * 3.14))**(1/3)) * 1e-6              !radius from volume
  n_d       = ((0.288 * (vol_d**0.811)) * 0.15e-6)/14               !N from volume

! ---------------------------- Picophytoplankton -------------------------------

  vol_p     = self%vP + self%vP * 0.025 * (15 - temp)               !volume calculation based on temp
  r_p       = (((vol_p * 3)/(4 * 3.14))**(1/3)) * 1e-6              !radius from volume
  n_p       = ((0.288 * (vol_p**0.811)) * 0.15e-6)/14               !N from volume


! ------------ Zooplankton N content as a function of size ---------------------

  n_zs       = (0.0546 + 0.0137 * ((4/3) * 3.14 * (self%r_zs * 1e3)**3)) * 1e3/14   !small zooplankton N content

  n_zl       = (0.0546 + 0.0137 * ((4/3) * 3.14 * (self%r_zl * 1e3)**3)) * 1e3/14   !large zooplankton N content

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion equations -----------------------------
! ----------------------------------- Viruses ----------------------------------

  d_v       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rV)

! ----------------------------------- Diatoms ----------------------------------

  d_di      = (self%kB * temp_K)/(6 * 3.14 * self%n * r_d)

! ------------------------------ Picophytoplankton -----------------------------

  d_p       = (self%kB * temp_K)/(6 * 3.14 * self%n * r_p)

! --------------------------- Zooplankton (swimming)---------------------------

  upreds     = exp(0.4 + 0.8 * log(self%r_zs * 200)) * 0.01           !swimming small zooplankton

  upredl     = exp(0.4 + 0.8 * log(self%r_zl * 200)) * 0.01           !swimming large zooplankton

! ---------------------------------- Zooplankton -------------------------------

  d_zs       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%r_zs)     !diffusion of small zooplankton

  d_zl       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%r_zl)     !diffusion of large zooplankton
! ---------------------------------- Detritus ----------------------------------

  d_d       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rD)

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion based Contact -------------------------
! ------------------------------ Viruses and Diatoms ---------------------------

  c_vdi     = 4 * 3.14 * (r_d + self%rV) * (d_v + d_di) *86400

! ------------------------- Viruses and Picophytoplankton ----------------------

  c_vp      = 4 * 3.14 * (r_p + self%rV) * (d_v + d_p) *86400

! ------------------------------ Viruses and Detritus --------------------------

  c_vd      = 4 * 3.14 * (self%rD + self%rV) * (d_v + d_d) *86400

! --------------------------- Zooplankton and Diatoms --------------------------

  c_zd      = ((4 * 3.14 * (d_di + d_zl) * (r_d + self%r_zl)) + 3.14 * ((r_d + 3 * self%r_zl)**2) * upredl) *86400

! ---------------------- Small Zooplankton and Picophytoplankton ---------------

  c_zp      = ((4 * 3.14 * (d_p + d_zs) * (r_p + self%r_zs)) + 3.14 * ((r_p + 3 * self%r_zs)**2) * upreds) *86400

! ------------------------------------------------------------------------------
! ------------------- Contact based Grazing, Infection, Decay ------------------
! ---------------------------------- Grazing -----------------------------------
! --------------------------- Zooplankton and Diatoms --------------------------

  g_d       = c_zd * (zl + self%z0) * (dia + self%p0) * (1/n_zl) * self%p_capd

! ---------------------- Zooplankton and Picophytoplankton ---------------------

  g_p       = c_zp * (zs + self%z0) * (pico + self%p0) * (1/n_zs) * self%p_capp

! ----------------------- Zooplankton and Infected Diatoms ---------------------

  g_infd    = c_zd * (zl + self%z0) * infd * (1/n_zl) * self%p_capd

! ------------------ Zooplankton and Infected Picophytoplankton ----------------

  g_infp    = c_zp * (zs + self%z0) * infp * (1/n_zs) * self%p_capp

! --------------------------------- Infection ----------------------------------
! ------------------------------ Viruses and Diatoms ---------------------------

  inf_d     = c_vdi * (dia + self%p0) * vd * (1/self%nV) * self%p_inf * self%p_ads

! ------------------------- Viruses and Picophytoplankton ----------------------

  inf_p     = c_vp * (pico + self%p0) * vp * (1/self%nV) * self%p_inf * self%p_ads

! ----------------------------------- Decay ------------------------------------
! ------------------------------ Viruses and Detritus --------------------------

  ads_vd    = c_vd * d * vd * (1/self%nD) * self%p_ads
  ads_vp    = c_vd * d * vp * (1/self%nD) * self%p_ads

! ------------------------------------------------------------------------------
! ---------------------------- Limitation Equations ----------------------------
  a_t       = 1
  b_d       = temp - self%Topt_d
  b_p       = temp - self%Topt_p
  i_limp    = 1 - exp(-0.07*par/1.0)                              !light limitation for phytoplankton growth also used for burst size

! ----------------------------------- Diatoms ----------------------------------

  n_limd    = n/(self%knd + n)
  t_limd    = exp(-((temp - self%Topt_d)**2)/((self%T1_d - self%T2_d * SIGN(a_t,b_d))**2))

! ------------------------------ Picophytoplankton -----------------------------

  n_limp    = n/(self%knp + n)
  t_limp    = exp(-((temp - self%Topt_p)**2)/((self%T1_p - self%T2_p * SIGN(a_t,b_p))**2))

! ----------------------------------- Viruses ----------------------------------

  b_limd     = i_limp * n_limd * t_limd                            !burst lim for diatoms as function of diatom growth limitations
  b_limp     = i_limp * n_limp * t_limp                            !burst lim for picophytoplankton as function of picophytoplankton growth limitations
  prop_vd    = (self%Bmin + self%Bmax * b_limd) * (self%nV / n_d)  !proportion of diatom cells converted to viral progeny in relation to burst size and N content
  prop_vp    = (self%Bmin + self%Bmax * b_limp) * (self%nV / n_p)  !proportion of picophytoplankton cells converted to viral progeny in relation to burst size and N content
  i_decay_vd = self%mV * (1 - exp(-0.1*par/3.0)) * vd              !decay of diatom viruses from radiation
  i_decay_vp = self%mV * (1 - exp(-0.1*par/3.0)) * vp              !decay of picophytoplankton viruses from radiation

! ------------------------------------------------------------------------------
! ----------------------------- Primary Production -----------------------------
! ----------------------------------- Diatoms ----------------------------------

  pp_d = self%gP_d * i_limp * n_limd * t_limd * (dia + self%p0)

! ------------------------------ Picophytoplankton -----------------------------

  pp_p = self%gP_p * i_limp * n_limp * t_limp * (pico + self%p0)

! ------------------------------------------------------------------------------
! ------------------------- Source/Sinks for Nutrients -------------------------

  dn = (self%rpn * dia) + (self%rpn * pico) + (self%wZ * zl) + (self%wZ * zs) + (self%R * d) - pp_d - pp_p

! ------------------------------------------------------------------------------
! -------------- Assign Destruction Rates to Destruction Matrices --------------

  _SET_DD_SYM_(self%id_n,       self%id_dia,    pp_d)                                                     ! sndi
  _SET_DD_SYM_(self%id_n,       self%id_pico,   pp_p)                                                     ! snp
  _SET_DD_SYM_(self%id_dia,     self%id_zl,     self%eZ * g_d)                                            ! sdiz
  _SET_DD_SYM_(self%id_pico,    self%id_zs,     self%eZ * g_p)                                            ! spz
  _SET_DD_SYM_(self%id_dia,     self%id_infd,   inf_d)                                                    ! sdiinfd
  _SET_DD_SYM_(self%id_pico,    self%id_infp,   inf_p)                                                    ! spinfp
  _SET_DD_SYM_(self%id_dia,     self%id_n,      self%rpn * dia)                                           ! sdin
  _SET_DD_SYM_(self%id_pico,    self%id_n,      self%rpn * pico)                                          ! spn
  _SET_DD_SYM_(self%id_dia,     self%id_d,      rpd*dia + (1-self%eZ) * g_d)                              ! sdid
  _SET_DD_SYM_(self%id_pico,    self%id_d,      rpd*pico + (1-self%eZ) * g_p)                             ! spd
  _SET_DD_SYM_(self%id_infd,    self%id_zl,     self%eZ * g_infd)                                         ! sinfdz
  _SET_DD_SYM_(self%id_infp,    self%id_zs,     self%eZ * g_infp)                                         ! sinfpz
  _SET_DD_SYM_(self%id_infd,    self%id_d,      (1-self%eZ) * g_infd + (1-prop_vd) * self%mPi * infd)     ! sinfdd
  _SET_DD_SYM_(self%id_infp,    self%id_d,      (1-self%eZ) * g_infp + (1-prop_vp) * self%mPi * infp)     ! sinfpd
  _SET_DD_SYM_(self%id_infd,    self%id_vd,     prop_vd * self%mPi * infd)                                ! sinfdvd
  _SET_DD_SYM_(self%id_infp,    self%id_vp,     prop_vp * self%mPi * infp)                                ! sinfpvp
  _SET_DD_SYM_(self%id_zs,      self%id_n,      self%wZ * zs)                                             ! szn
  _SET_DD_SYM_(self%id_zs,      self%id_d,      self%mZ * (zs**2))                                        ! szd
  _SET_DD_SYM_(self%id_zl,      self%id_n,      self%wZ * zl)                                             ! szn
  _SET_DD_SYM_(self%id_zl,      self%id_d,      self%mZ * (zl**2))                                        ! szd
  _SET_DD_SYM_(self%id_d,       self%id_n,      self%R * d)                                               ! sdn
  _SET_DD_SYM_(self%id_vd,      self%id_d,      ads_vd + i_decay_vd)                                      ! svd
  _SET_DD_SYM_(self%id_vp,      self%id_d,      ads_vp + i_decay_vp)                                      ! svd

! ------------------------------------------------------------------------------
! ----------------- DIC pool change according to change in nut -----------------

  if (_AVAILABLE_(self%id_dic)) _SET_PP_(self%id_dic,self%id_dic,self%dic_per_n*dn)

! ------------------------------------------------------------------------------
! ------------------------- Export diagnostic variables ------------------------

  _SET_DIAGNOSTIC_(self%id_dPAR,  par)
  _SET_DIAGNOSTIC_(self%id_GPP ,  pp_d + pp_p)
  _SET_DIAGNOSTIC_(self%id_NCP ,  pp_d + pp_p - self%rpn * dia - self%rpn * pico)
  _SET_DIAGNOSTIC_(self%id_PPR ,  (pp_d + pp_p) * secs_pr_day)
  _SET_DIAGNOSTIC_(self%id_NPR ,  (pp_d + pp_p - self%rpn * dia - self%rpn * pico) * secs_pr_day)

! ---------------------------------- End Loop ----------------------------------

  _LOOP_END_

  end subroutine do_ppdd

! ------------------------------------------------------------------------------

  end module gotm_npzd
