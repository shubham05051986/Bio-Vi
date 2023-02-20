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

    type (type_state_variable_id)         :: id_n, id_phy, id_z, id_v, id_inf, id_d
    type (type_state_variable_id)         :: id_dic
    type (type_dependency_id)             :: id_par, id_temp
    type (type_horizontal_dependency_id)  :: id_I_0
    type (type_diagnostic_variable_id)    :: id_GPP, id_NCP, id_PPR, id_NPR, id_dPAR

!-------------------------------------------------------------------------------
! --------------------------- Model Parameters ---------------------------------

    real (rk) :: p0, z0, kc, i_min, R, kn, rpn
    real (rk) :: gP, rpdu, rpdl, mPi, eZ, mZ, wZ, r_z, mV, rV
    real (rk) :: vP, rD, nV, nD, kB, n, p_ads, p_inf
    real (rk) :: p_cap, Bmin, Bmax, Topt, T1, T2
    real (rk) :: w_p, w_d, dic_per_n
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
  real(rk)          :: phy_initial
  real(rk)          :: z_initial
  real(rk)          :: v_initial
  real(rk)          :: inf_initial
  real(rk)          :: d_initial
  real(rk)          :: p0
  real(rk)          :: z0
  real(rk)          :: kc
  real(rk)          :: i_min
  real(rk)          :: R
  real(rk)          :: kn
  real(rk)          :: rpn
  real(rk)          :: gP
  real(rk)          :: rpdu
  real(rk)          :: rpdl
  real(rk)          :: mPi
  real(rk)          :: eZ
  real(rk)          :: mZ
  real(rk)          :: wZ
  real(rk)          :: r_z
  real(rk)          :: mV
  real(rk)          :: rV
  real(rk)          :: vP
  real(rk)          :: rD
  real(rk)          :: nV
  real(rk)          :: nD
  real(rk)          :: kB
  real(rk)          :: n
  real(rk)          :: p_ads
  real(rk)          :: p_inf
  real(rk)          :: p_cap
  real(rk)          :: Bmin
  real(rk)          :: Bmax
  real(rk)          :: Topt
  real(rk)          :: T1
  real(rk)          :: T2
  real(rk)          :: w_p
  real(rk)          :: w_d
  real(rk)          :: dic_per_n
  character(len=64) :: dic_variable

  real(rk), parameter :: d_per_s = 1.0_rk/86400.0_rk
  namelist /gotm_npzd/ n_initial,phy_initial,z_initial,v_initial,         &
                       inf_initial,d_initial,p0,z0,kc,i_min,R,kn,   &
                       rpn,gP,rpdu,rpdl,mPi,eZ,mZ,wZ,r_z,mV,                &
                       rV,vP,rD,nV,nD,kB,n,p_ads,p_inf,p_cap,         &
                       Bmin,Bmax,Topt,T1,T2,w_p,w_d,dic_per_n

!-------------------------------------------------------------------------------

  n_initial      = 0.0_rk
  phy_initial    = 0.0_rk
  z_initial      = 0.0_rk
  v_initial      = 0.0_rk
  inf_initial    = 0.0_rk
  d_initial      = 0.00_rk
  p0             = 0.0_rk
  z0             = 0.0_rk
  kc             = 0.0_rk
  i_min          = 0.0_rk
  R              = 0.0_rk
  kn             = 0.0_rk
  rpn            = 0.0_rk
  gP             = 0.0_rk
  rpdu           = 0.0_rk
  rpdl           = 0.0_rk
  mPi            = 0.0_rk
  eZ             = 0.0_rk
  mZ             = 0.0_rk
  wZ             = 0.0_rk
  r_z            = 0.0_rk
  mV             = 0.0_rk
  rV             = 0.0_rk
  vP             = 0.0_rk
  rD             = 0.0_rk
  nV             = 0.0_rk
  nD             = 0.0_rk
  kB             = 0.0_rk
  n              = 0.0_rk
  p_ads          = 0.0_rk
  p_inf          = 0.0_rk
  p_cap          = 0.0_rk
  Bmin           = 0.0_rk
  Bmax           = 0.0_rk
  Topt           = 0.0_rk
  T1             = 0.0_rk
  T2             = 0.0_rk
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
  call self%get_parameter(self%kn,       'kn',       'mmol m-3',    'half saturation constant Phytoplankton',default=kn)
  call self%get_parameter(self%rpn,      'rpn',      'd-1',         'loss rate of phytoplankton to nutrients',default=rpn,scale_factor=d_per_s)
  call self%get_parameter(self%gP,       'gP',       'd-1',         'growth rate Phytoplankton',default=gP,scale_factor=d_per_s)
  call self%get_parameter(self%rpdu,     'rpdu',     'd-1',         'phytoplankton mortality in euphotic zone',default=rpdu,scale_factor=d_per_s)
  call self%get_parameter(self%rpdl,     'rpdl',     'd-1',         'phytoplankton mortality below euphotic zone',default=rpdl,scale_factor=d_per_s)
  call self%get_parameter(self%mPi,      'mPi',      'd-1',         'lysis rate of infected phytoplankton',default=mPi,scale_factor=d_per_s)
  call self%get_parameter(self%eZ,       'eZ',       '-',           'efficiency rate zooplankton grazing',default=eZ)
  call self%get_parameter(self%mZ,       'mZ',       'd-1',         'zooplankton mortality',default=mZ,scale_factor=d_per_s)
  call self%get_parameter(self%wZ,       'wZ',       'd-1',         'loss rate of zooplankton to nutrients',default=wZ,scale_factor=d_per_s)
  call self%get_parameter(self%r_z,      'r_z',      'm',           'radius of zooplankton particles',default=r_z)
  call self%get_parameter(self%mV,       'mV',       'd-1',         'decay of viruses',default=mV,scale_factor=d_per_s)
  call self%get_parameter(self%rV,       'rV',       'm',           'radius of viruses particle',default=rV)
  call self%get_parameter(self%vP,       'vP',       'µm^3',        'volume of diatom cell at 15°',default=vP)
  call self%get_parameter(self%rD,       'rD',       'm',           'radius of detritus particles',default=rD)
  call self%get_parameter(self%nV,       'nV',       'mmol N',      'N content of viruses particle',default=nV)
  call self%get_parameter(self%nD,       'nD',       'mmol N',      'N content of detritus particles',default=nD)
  call self%get_parameter(self%kB,       'kB',       'g m2 s-2 K-1','Boltzman Constant',default=kB)
  call self%get_parameter(self%n,        'n',        'g m2 s-1',    'dynamic viscosity',default=n)
  call self%get_parameter(self%p_ads,    'p_ads',    '-',           'probability of adhesion',default=p_ads)
  call self%get_parameter(self%p_inf,    'p_inf',    '-',           'probability of infection',default=p_inf)
  call self%get_parameter(self%p_cap,    'p_cap',    '-',           'probability of capture phy',default=p_cap)
  call self%get_parameter(self%Bmin,     'Bmin',     'particles',   'minimum burst size',default=Bmin)
  call self%get_parameter(self%Bmax,     'Bmax',     'particles',   'maximum burst size',default=Bmax)
  call self%get_parameter(self%Topt,     'Topt',     '°C',          'temperature optimum for diatom growth',default=Topt)
  call self%get_parameter(self%T1,       'T1',       '°C',          'width parameter of reaction norm phy',default=T1)
  call self%get_parameter(self%T2,       'T2',       '°C',          'width parameter of reaction norm phy',default=T2)
  call self%get_parameter(w_p,           'w_p',      'm d-1',       'vertical velocity of Phytoplankton (<0 for sinking)',default=w_p, scale_factor=d_per_s)
  call self%get_parameter(w_d,           'w_d',      'm d-1',       'vertical velocity of detritus  (<0 for sinking)',default=w_d,scale_factor=d_per_s)
  call self%get_parameter(self%dic_per_n,'dic_per_n','-',           'C:N ratio of biomass',default=dic_per_n)

! ------------------------------------------------------------------------------
! ------------------------ Register State Variables ----------------------------

  call self%register_state_variable(self%id_n,      'nut',      'mmol m-3','nutrients',                 n_initial,      minimum=0.0_rk, no_river_dilution=.true.)
  call self%register_state_variable(self%id_phy,    'phy',      'mmol m-3','phytoplankton',             phy_initial,    minimum=0.0_rk, vertical_movement=w_p)
  call self%register_state_variable(self%id_z,      'zoo',      'mmol m-3','zooplankton',               z_initial,      minimum=0.0_rk)
  call self%register_state_variable(self%id_d,      'det',      'mmol m-3','detritus',                  d_initial,      minimum=0.0_rk, vertical_movement=w_d)
  call self%register_state_variable(self%id_v,      'vir',      'mmol m-3','viruses',                   v_initial,      minimum=0.0_rk)
  call self%register_state_variable(self%id_inf,    'inf',      'mmol m-3','infected Phytoplankton',    inf_initial,   minimum=0.0_rk, vertical_movement=w_p)

! ------------------------------------------------------------------------------
! ----------------- Register contribution to total Nitrogen --------------------

  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_n)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_phy)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_z)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_d)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_v)
  call self%add_to_aggregate_variable(standard_variables%total_nitrogen,  self%id_inf)

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

  real(rk)                   :: n, phy, z, d, v, inf, par, I_0, temp
  real(rk)                   :: temp_K, rpd, vol_p, r_p, n_p
  real(rk)                   :: r_pz, r_z, n_z, d_v, d_p, upred, d_z, d_d
  real(rk)                   :: c_vp, c_vd, c_zd, g_p, g_inf
  real(rk)                   :: inf_p,inf_v, ads, a_t, b_d, i_lim, n_lim, t_lim
  real(rk)                   :: b_limd, prop_vp, i_decay_v
  real(rk)                   :: pp_p, dn
  real(rk), parameter        :: secs_pr_day = 86400.

! ------------------------------------------------------------------------------
! --------------------------------- Begin Loop ---------------------------------

  _LOOP_BEGIN_

! ------------------------------------------------------------------------------
! --------------- Retrieve current (local) state variable values ---------------

  _GET_(self%id_n,    n)             ! nutrient
  _GET_(self%id_phy,  phy)           ! Phytoplankton
  _GET_(self%id_z,    z)             ! zooplankton
  _GET_(self%id_d,    d)             ! detritus
  _GET_(self%id_v,    v)             ! viruses
  _GET_(self%id_inf, inf)            ! infected Phytoplankton

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
! ---------------------------------- Phytoplankton -----------------------------

  vol_p     = self%vP + self%vP * 0.025 * (15 - temp)         ! temperature dependent phytoplankton volume
  r_p       = (((vol_p * 3)/(4 * 3.14))**(1/3))*1e-6          ! radius based on volume
  n_p       = ((0.288 * (vol_p**0.811)) * 0.15)*1e-6/14       ! N content based on volume

! ------------------ Zooplankton N content based on size -----------------------

  n_z       = (0.0546 + 0.0137 * ((4/3) * 3.14 * (self%r_z * 1e3)**3))*1e3/14

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion equations -----------------------------
! ----------------------------------- Viruses ----------------------------------

  d_v       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rV)

! ------------------------------- Phytoplankton --------------------------------

  d_p      = (self%kB * temp_K)/(6 * 3.14 * self%n * r_p)

! --------------------------- Zooplankton (swimming)----------------------------

  upred     = exp(0.4 + 0.8 * log(self%r_z * 200)) * 0.01

! ---------------------------------- Detritus ----------------------------------

  d_z       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%r_z)

! ---------------------------------- Detritus ----------------------------------

  d_d       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rD)

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion based Contact -------------------------
! ------------------------------ Viruses and Phytoplankton ---------------------

  c_vp     = 4 * 3.14 * (r_p + self%rV) * (d_v + d_p)*86400

! ------------------------------ Viruses and Detritus --------------------------

  c_vd      = 4 * 3.14 * (self%rD + self%rV) * (d_v + d_d)*86400

! --------------------------- Zooplankton and Phytoplankton --------------------

  c_zd      = ((4 * 3.14 * (d_p + d_z) * (r_p + self%r_z)) + 3.14 * ((r_p + 3 * self%r_z)**2) * upred)*86400

! ------------------------------------------------------------------------------
! ------------------- Contact based Grazing, Infection, Decay ------------------
! ---------------------------------- Grazing -----------------------------------
! --------------------------- Zooplankton and Phytoplankton --------------------

  g_p       = c_zd * (z + self%z0) * (phy + self%p0) * (1/n_z) * self%p_cap

! ----------------------- Zooplankton and Infected Phytoplankton ---------------

  g_inf    = c_zd * (z + self%z0) * inf * (1/n_z) * self%p_cap

! --------------------------------- Infection ----------------------------------
! -------------------------- Viruses and Phytoplankton -------------------------

  inf_p     = c_vp * (phy + self%p0) * v * (1/self%nV) * self%p_inf * self%p_ads      !phytoplankton successfully infected

  inf_v     = c_vp * (phy + self%p0) * v * (1/n_p) * self%p_inf * self%p_ads          !viruses that infect

! ----------------------------------- Decay ------------------------------------
! ------------------------------ Viruses and Detritus --------------------------

  ads       = c_vd * d * v * (1/self%nD) * self%p_ads

! ------------------------------------------------------------------------------
! ---------------------------- Limitation Equations ----------------------------
  a_t       = 1
  b_d       = temp - self%Topt
  i_lim    = 1 - exp(-0.07*par/1.0)                 ! light limitation for phytoplankton growth also used for burst size

! ----------------------------------- Phytoplankton ----------------------------------

  n_lim    = n/(self%kn + n)
  t_lim    = exp(-((temp - self%Topt)**2)/((self%T1 - self%T2 * SIGN(a_t,b_d))**2))

! ----------------------------------- Viruses ----------------------------------

  b_limd    = i_lim * n_lim * t_lim                                 !burst size limitations as function of phytoplankton growth limitations
  prop_vp   = (self%Bmin + self%Bmax * b_limd) * (self%nV / n_p)    !proportion of phytoplankton released as viral progeny in relation to burst size
  i_decay_v = self%mV * (1 - exp(-0.1*par/3.0)) * v                 !viral decay influenced by radiation

! ------------------------------------------------------------------------------
! ----------------------------- Primary Production -----------------------------
! ----------------------------------- Phytoplankton ----------------------------

  pp_p = self%gP * i_lim * n_lim * t_lim * (phy + self%p0)

! ------------------------------------------------------------------------------
! ------------------------- Source/Sinks for Nutrients -------------------------

  dn = (self%rpn * phy) + (self%wZ * z) + (self%R * d) - pp_p

! ------------------------------------------------------------------------------
! -------------------------- Set temporal derivatives --------------------------

  _SET_ODE_(self%id_n,        dn)
  _SET_ODE_(self%id_phy,      pp_p - g_p - (self%rpn * phy) - (rpd * phy) - inf_p)
  _SET_ODE_(self%id_z,        self%eZ * (g_p + g_inf) - (self%wZ * z) - (self%mZ * (z**2)))
  _SET_ODE_(self%id_d,        rpd * phy + self%mZ * (z**2) + (1-self%eZ) * (g_p + g_inf) + (1-prop_vp)*self%mPi*inf + ads - self%R*d)
  _SET_ODE_(self%id_v,        prop_vp * self%mPi * inf - i_decay_v - ads - inf_v)
  _SET_ODE_(self%id_inf,      inf_p + inf_v - self%mPi * inf - g_inf)

! ------------------------------------------------------------------------------
! ----------------- DIC pool change according to change in nut -----------------

  if (_AVAILABLE_(self%id_dic)) _SET_ODE_(self%id_dic,self%dic_per_n*dn)

! ------------------------------------------------------------------------------
! ------------------------- Export diagnostic variables ------------------------

  _SET_DIAGNOSTIC_(self%id_dPAR,  par)
  _SET_DIAGNOSTIC_(self%id_GPP ,  pp_p)
  _SET_DIAGNOSTIC_(self%id_NCP ,  pp_p - self%rpn * phy)
  _SET_DIAGNOSTIC_(self%id_PPR ,  inf_p/phy * secs_pr_day)
  _SET_DIAGNOSTIC_(self%id_NPR ,  g_p/phy * secs_pr_day)

! ------------------------------------------------------------------------------
! ---------------------------------- End Loop ----------------------------------

  _LOOP_END_

  end subroutine do

! ------------------------------------------------------------------------------
! ------------------------------ Light Extinction ------------------------------

   subroutine get_light_extinction(self,_ARGUMENTS_GET_EXTINCTION_)

   class (type_gotm_npzd), intent(in) :: self
   _DECLARE_ARGUMENTS_GET_EXTINCTION_

  real(rk)                            :: phy,d

! ------------------------------------------------------------------------------
! -------------------------------- Begin Loop ----------------------------------

  _LOOP_BEGIN_

  _GET_(self%id_phy,  phy)   ! Phytoplankton
  _GET_(self%id_d,    d)     ! detritus

  _SET_EXTINCTION_(self%kc * (self%p0 + phy + d))

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

  real(rk)                   :: n, phy, z, d, v, inf, par, I_0, temp
  real(rk)                   :: temp_K, rpd, vol_p, r_p, n_p
  real(rk)                   :: r_pz, r_z, n_z, d_v, d_p, upred, d_z, d_d
  real(rk)                   :: c_vp, c_vd, c_zd, g_p, g_inf
  real(rk)                   :: inf_p, inf_v, ads, a_t, b_d, i_lim, n_lim, t_lim
  real(rk)                   :: b_limd, prop_vp, i_decay_v
  real(rk)                   :: pp_p, dn
  real(rk), parameter        :: secs_pr_day = 86400.

! ------------------------------------------------------------------------------
! --------------------------------- Begin Loop ---------------------------------

  _LOOP_BEGIN_

! ------------------------------------------------------------------------------
! --------------- Retrieve current (local) state variable values ---------------
  _GET_(self%id_n,    n)             ! nutrient
  _GET_(self%id_phy,  phy)           ! Phytoplankton
  _GET_(self%id_z,    z)             ! zooplankton
  _GET_(self%id_d,    d)             ! detritus
  _GET_(self%id_v,    v)             ! viruses
  _GET_(self%id_inf, inf)          ! infected Phytoplankton

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
! ---------------------------------- Phytoplankton -----------------------------

  vol_p     = self%vP + self%vP * 0.025 * (15 - temp)         ! temperature dependent phytoplankton volume
  r_p       = (((vol_p * 3)/(4 * 3.14))**(1/3))*1e-6          ! radius based on volume
  n_p       = ((0.288 * (vol_p**0.811)) * 0.15)*1e-6/14       ! N content based on volume

! ------------------ Zooplankton N content based on size -----------------------

  n_z       = (0.0546 + 0.0137 * ((4/3) * 3.14 * (self%r_z * 1e3)**3))*1e3/14

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion equations -----------------------------
! ----------------------------------- Viruses ----------------------------------

  d_v       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rV)

! ----------------------------------- Phytoplankton ----------------------------

  d_p      = (self%kB * temp_K)/(6 * 3.14 * self%n * r_p)

! --------------------------- Zooplankton (swimming)----------------------------

  upred     = exp(0.4 + 0.8 * log(self%r_z * 200)) * 0.01

! --------------------------- Zooplankton (diffusion)---------------------------

  d_z       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%r_z)

! ---------------------------------- Detritus ----------------------------------

  d_d       = (self%kB * temp_K)/(6 * 3.14 * self%n * self%rD)

! ------------------------------------------------------------------------------
! ---------------------------- Diffusion based Contact -------------------------
! --------------------------- Viruses and Phytoplankton ------------------------

  c_vp     = 4 * 3.14 * (r_p + self%rV) * (d_v + d_p)*86400

! ------------------------------ Viruses and Detritus --------------------------

  c_vd      = 4 * 3.14 * (self%rD + self%rV) * (d_v + d_d)*86400

! --------------------------- Zooplankton and Phytoplankton --------------------

  c_zd      = ((4 * 3.14 * (d_p + d_z) * (r_p + self%r_z)) + 3.14 * ((r_p + 3 * self%r_z)**2) * upred)*86400

! ------------------------------------------------------------------------------
! ------------------- Contact based Grazing, Infection, Decay ------------------
! ---------------------------------- Grazing -----------------------------------
! ------------------------- Zooplankton and Phytoplankton ----------------------

  g_p       = c_zd * (z + self%z0) * (phy + self%p0) * (1/n_z) * self%p_cap

! ----------------------- Zooplankton and Infected Phytoplankton ---------------

  g_inf    = c_zd * (z + self%z0) * inf * (1/n_z) * self%p_cap

! --------------------------------- Infection ----------------------------------
! ------------------------------ Viruses and Phytoplankton ---------------------

  inf_p     = c_vp * (phy + self%p0) * v * (1/self%nV) * self%p_inf * self%p_ads

  inf_v     = c_vp * (phy + self%p0) * v * (1/n_p) * self%p_inf * self%p_ads
! ----------------------------------- Decay ------------------------------------
! ------------------------------ Viruses and Detritus --------------------------

  ads       = c_vd * d * v * (1/self%nD) * self%p_ads

! ------------------------------------------------------------------------------
! ---------------------------- Limitation Equations ----------------------------
  a_t       = 1
  b_d       = temp - self%Topt
  i_lim    = 1 - exp(-0.07*par/1.0)                 ! light limitation for phytoplankton growth also used for burst size

! ----------------------------------- Phytoplankton ----------------------------

  n_lim    = n/(self%kn + n)
  t_lim    = exp(-((temp - self%Topt)**2)/((self%T1 - self%T2 * SIGN(a_t,b_d))**2))

! ----------------------------------- Viruses ----------------------------------

  b_limd    = i_lim * n_lim * t_lim                                 !burst size limitations as function of phytoplankton growth limitations
  prop_vp   = (self%Bmin + self%Bmax * b_limd) * (self%nV / n_p)    !proportion of phytoplankton released as viral progeny in relation to burst size
  i_decay_v = self%mV * (1 - exp(-0.1*par/3.0)) * v                 !viral decay influenced by radiation

! ------------------------------------------------------------------------------
! ----------------------------- Primary Production -----------------------------
! ----------------------------------- Phytoplankton ----------------------------

  pp_p = self%gP * i_lim * n_lim * t_lim * (phy + self%p0)

! ------------------------------------------------------------------------------
! ------------------------- Source/Sinks for Nutrients -------------------------

  dn = (self%rpn * phy) + (self%wZ * z) + (self%R * d) - pp_p

! ------------------------------------------------------------------------------
! -------------- Assign Destruction Rates to Destruction Matrices --------------

  _SET_DD_SYM_(self%id_n,       self%id_phy,    pp_p)                                                    ! sndi
  _SET_DD_SYM_(self%id_phy,     self%id_z,      self%eZ * g_p)                                           ! sdiz
  _SET_DD_SYM_(self%id_phy,     self%id_inf,    inf_p)                                                   ! sdiinfd
  _SET_DD_SYM_(self%id_phy,     self%id_n,      self%rpn * phy)                                          ! sdin
  _SET_DD_SYM_(self%id_phy,     self%id_d,      rpd*phy + (1-self%eZ) * g_p)                             ! sdid
  _SET_DD_SYM_(self%id_inf,     self%id_z,      self%eZ * g_inf)                                         ! sinfdz
  _SET_DD_SYM_(self%id_inf,     self%id_d,      (1-self%eZ) * g_inf + (1-prop_vp) * self%mPi * inf)      ! sinfdd
  _SET_DD_SYM_(self%id_inf,     self%id_v,      prop_vp * self%mPi * inf)                                ! sinfdv
  _SET_DD_SYM_(self%id_z,       self%id_n,      self%wZ * z)                                             ! szn
  _SET_DD_SYM_(self%id_z,       self%id_d,      self%mZ * (z**2))                                        ! szd
  _SET_DD_SYM_(self%id_d,       self%id_n,      self%R * d)                                              ! sdn
  _SET_DD_SYM_(self%id_v,       self%id_d,      ads + i_decay_v)                                         ! svd
  _SET_DD_SYM_(self%id_v,       self%id_inf,      inf_v)                                                 ! svinf

! ------------------------------------------------------------------------------
! ----------------- DIC pool change according to change in nut -----------------

  if (_AVAILABLE_(self%id_dic)) _SET_PP_(self%id_dic,self%id_dic,self%dic_per_n*dn)

! ------------------------------------------------------------------------------
! ------------------------- Export diagnostic variables ------------------------

  _SET_DIAGNOSTIC_(self%id_dPAR,  par)
  _SET_DIAGNOSTIC_(self%id_GPP ,  pp_p)
  _SET_DIAGNOSTIC_(self%id_NCP ,  pp_p - self%rpn * phy)
  _SET_DIAGNOSTIC_(self%id_PPR ,  inf_p/phy * secs_pr_day)
  _SET_DIAGNOSTIC_(self%id_NPR ,  g_p/phy * secs_pr_day)

! ---------------------------------- End Loop ----------------------------------

  _LOOP_END_

  end subroutine do_ppdd

! ------------------------------------------------------------------------------

  end module gotm_npzd
