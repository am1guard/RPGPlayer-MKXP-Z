/*
 ** binding-mri.cpp
 **
 ** This file is part of mkxp.
 **
 ** Copyright (C) 2013 - 2021 Amaryllis Kulla <ancurio@mapleshrine.eu>
 **
 ** mkxp is free software: you can redistribute it and/or modify
 ** it under the terms of the GNU General Public License as published by
 ** the Free Software Foundation, either version 2 of the License, or
 ** (at your option) any later version.
 **
 ** mkxp is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ** GNU General Public License for more details.
 **
 ** You should have received a copy of the GNU General Public License
 ** along with mkxp.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "audio/audio.h"
#include "filesystem/filesystem.h"
#include "display/graphics.h"
#include "display/font.h"
#include "system/system.h"

#include "util/util.h"
#include "util/sdl-util.h"
#include "util/debugwriter.h"
#include "util/boost-hash.h"
#include "util/exception.h"
#include "util/encoding.h"

#include "config.h"

#include "binding-util.h"
#include "binding.h"

#include "sharedstate.h"
#include "eventthread.h"

#include <vector>
#include "util/rapidcsv.h"

extern "C" {
#include <ruby.h>

#if RAPI_FULL >= 190
#include <ruby/encoding.h>
#endif
}

#ifdef __WIN32__
#include "binding-mri-win32.h"
#endif

#include <assert.h>
#include <string>
#include <regex>
#include <zlib.h>
#include <ctime>
#include <cstdarg>
#include <cstdlib>

#include <SDL_cpuinfo.h>
#include <SDL_filesystem.h>
#include <SDL_loadso.h>
#include <SDL_power.h>

extern const char module_rpg1[];
extern const char module_rpg2[];
extern const char module_rpg3[];

static VALUE topSelf;

static void mriBindingExecute();
static void mriBindingTerminate();
static void mriBindingReset();

ScriptBinding scriptBindingImpl = {mriBindingExecute, mriBindingTerminate,
    mriBindingReset};

ScriptBinding *scriptBinding = &scriptBindingImpl;

void tableBindingInit();
void etcBindingInit();
void fontBindingInit();
void bitmapBindingInit();
void spriteBindingInit();
void viewportBindingInit();
void planeBindingInit();
void windowBindingInit();
void tilemapBindingInit();
void windowVXBindingInit();
void tilemapVXBindingInit();

void inputBindingInit();
void audioBindingInit();
void graphicsBindingInit();

void fileIntBindingInit();

// Forward declarations for static Ruby extensions (C functions from libruby.a)
extern "C" void Init_zlib(void);
extern "C" void Init_enc(void);
extern "C" void Init_Encoding(void);
extern "C" void Init_encodings(void);

#ifdef MKXPZ_MINIFFI
void MiniFFIBindingInit();
#endif

#ifdef MKXPZ_STEAM
void CUSLBindingInit();
#endif

void httpBindingInit();

RB_METHOD(mkxpDelta);
RB_METHOD(mriPrint);
RB_METHOD(mriP);
RB_METHOD(mkxpDataDirectory);
RB_METHOD(mkxpSetTitle);
RB_METHOD(mkxpGetTitle);
RB_METHOD(mkxpDesensitize);
RB_METHOD(mkxpPuts);

RB_METHOD(mkxpPlatform);
RB_METHOD(mkxpIsMacHost);
RB_METHOD(mkxpIsWindowsHost);
RB_METHOD(mkxpIsLinuxHost);
RB_METHOD(mkxpIsUsingRosetta);
RB_METHOD(mkxpIsUsingWine);
RB_METHOD(mkxpIsReallyMacHost);
RB_METHOD(mkxpIsReallyLinuxHost);
RB_METHOD(mkxpIsReallyWindowsHost);

RB_METHOD(mkxpUserLanguage);
RB_METHOD(mkxpUserName);
RB_METHOD(mkxpGameTitle);
RB_METHOD(mkxpPowerState);
RB_METHOD(mkxpSettingsMenu);
RB_METHOD(mkxpCpuCount);
RB_METHOD(mkxpSystemMemory);
RB_METHOD(mkxpReloadPathCache);
RB_METHOD(mkxpAddPath);
RB_METHOD(mkxpRemovePath);
RB_METHOD(mkxpFileExists);
RB_METHOD(mkxpLaunch);

RB_METHOD(mkxpGetJSONSetting);
RB_METHOD(mkxpSetJSONSetting);
RB_METHOD(mkxpGetAllJSONSettings);

RB_METHOD(mkxpSetDefaultFontFamily);

RB_METHOD(mriRgssMain);
RB_METHOD(mriRgssStop);
RB_METHOD(_kernelCaller);

RB_METHOD(mkxpStringToUTF8);
RB_METHOD(mkxpStringToUTF8Bang);

VALUE json2rb(json5pp::value const &v);
json5pp::value rb2json(VALUE v);

RB_METHOD(mkxpParseCSV);

static void mriBindingInit() {
    fprintf(stderr, "[MKXP-Z] DEBUG: mriBindingInit starting...\n");
    fprintf(stderr, "[MKXP-Z] DEBUG: tableBindingInit...\n");
    tableBindingInit();
    fprintf(stderr, "[MKXP-Z] DEBUG: etcBindingInit...\n");
    etcBindingInit();
    fontBindingInit();
    bitmapBindingInit();
    spriteBindingInit();
    viewportBindingInit();
    planeBindingInit();
    
    if (rgssVer == 1) {
        windowBindingInit();
        tilemapBindingInit();
    } else {
        windowVXBindingInit();
        tilemapVXBindingInit();
    }
    
    fprintf(stderr, "[MKXP-Z] DEBUG: inputBindingInit...\n");
    inputBindingInit();
    fprintf(stderr, "[MKXP-Z] DEBUG: audioBindingInit...\n");
    audioBindingInit();
    fprintf(stderr, "[MKXP-Z] DEBUG: graphicsBindingInit...\n");
    graphicsBindingInit();
    
    fprintf(stderr, "[MKXP-Z] DEBUG: fileIntBindingInit...\n");
    fileIntBindingInit();
    
#ifdef MKXPZ_MINIFFI
    MiniFFIBindingInit();
#endif
    
#ifdef MKXPZ_STEAM
    CUSLBindingInit();
#endif
    
    httpBindingInit();
    
    if (rgssVer >= 3) {
        _rb_define_module_function(rb_mKernel, "rgss_main", mriRgssMain);
        _rb_define_module_function(rb_mKernel, "rgss_stop", mriRgssStop);
        
        _rb_define_module_function(rb_mKernel, "msgbox", mriPrint);
        _rb_define_module_function(rb_mKernel, "msgbox_p", mriP);
        
        rb_define_global_const("RGSS_VERSION", rb_utf8_str_new_cstr("3.0.1"));
    } else {
        _rb_define_module_function(rb_mKernel, "print", mriPrint);
        _rb_define_module_function(rb_mKernel, "p", mriP);
        
        rb_define_alias(rb_singleton_class(rb_mKernel), "_mkxp_kernel_caller_alias",
                        "caller");
        _rb_define_module_function(rb_mKernel, "caller", _kernelCaller);
    }
    
    if (rgssVer == 1)
        rb_eval_string(module_rpg1);
    else if (rgssVer == 2)
        rb_eval_string(module_rpg2);
    else if (rgssVer == 3)
        rb_eval_string(module_rpg3);
    else
        assert(!"unreachable");
    
    VALUE mod = rb_define_module("System");
    _rb_define_module_function(mod, "delta", mkxpDelta);
    _rb_define_module_function(mod, "uptime", mkxpDelta);
    _rb_define_module_function(mod, "data_directory", mkxpDataDirectory);
    _rb_define_module_function(mod, "set_window_title", mkxpSetTitle);
    _rb_define_module_function(mod, "window_title", mkxpGetTitle);
    _rb_define_module_function(mod, "window_title=", mkxpSetTitle);
    _rb_define_module_function(mod, "show_settings", mkxpSettingsMenu);
    _rb_define_module_function(mod, "puts", mkxpPuts);
    _rb_define_module_function(mod, "desensitize", mkxpDesensitize);
    _rb_define_module_function(mod, "platform", mkxpPlatform);
    
    _rb_define_module_function(mod, "is_mac?", mkxpIsMacHost);
    _rb_define_module_function(mod, "is_rosetta?", mkxpIsUsingRosetta);
    
    _rb_define_module_function(mod, "is_linux?", mkxpIsLinuxHost);
    
    _rb_define_module_function(mod, "is_windows?", mkxpIsWindowsHost);
    _rb_define_module_function(mod, "is_wine?", mkxpIsUsingWine);
    _rb_define_module_function(mod, "is_really_mac?", mkxpIsReallyMacHost);
    _rb_define_module_function(mod, "is_really_linux?", mkxpIsReallyLinuxHost);
    _rb_define_module_function(mod, "is_really_windows?", mkxpIsReallyWindowsHost);
    
    
    _rb_define_module_function(mod, "user_language", mkxpUserLanguage);
    _rb_define_module_function(mod, "user_name", mkxpUserName);
    _rb_define_module_function(mod, "game_title", mkxpGameTitle);
    _rb_define_module_function(mod, "power_state", mkxpPowerState);
    _rb_define_module_function(mod, "nproc", mkxpCpuCount);
    _rb_define_module_function(mod, "memory", mkxpSystemMemory);
    _rb_define_module_function(mod, "reload_cache", mkxpReloadPathCache);
    _rb_define_module_function(mod, "mount", mkxpAddPath);
    _rb_define_module_function(mod, "unmount", mkxpRemovePath);
    _rb_define_module_function(mod, "file_exist?", mkxpFileExists);
    _rb_define_module_function(mod, "launch", mkxpLaunch);
    
    _rb_define_module_function(mod, "default_font_family=", mkxpSetDefaultFontFamily);
    
    _rb_define_module_function(mod, "parse_csv", mkxpParseCSV);
    
    _rb_define_method(rb_cString, "to_utf8", mkxpStringToUTF8);
    _rb_define_method(rb_cString, "to_utf8!", mkxpStringToUTF8Bang);
    
    VALUE cmod = rb_define_module("CFG");
    _rb_define_module_function(cmod, "[]", mkxpGetJSONSetting);
    _rb_define_module_function(cmod, "[]=", mkxpSetJSONSetting);
    _rb_define_module_function(cmod, "to_hash", mkxpGetAllJSONSettings);
    
    /* Load global constants */
    rb_gv_set("MKXP", Qtrue);
    
    VALUE debug = rb_bool_new(shState->config().editor.debug);
    if (rgssVer == 1)
        rb_gv_set("DEBUG", debug);
    else if (rgssVer >= 2)
        rb_gv_set("TEST", debug);
    
    rb_gv_set("BTEST", rb_bool_new(shState->config().editor.battleTest));

    // Suppress verbose warnings from game scripts (e.g. constant redefinition)
    rb_gv_set("VERBOSE", Qnil);
    
#ifdef MKXPZ_BUILD_XCODE
    std::string version = std::string(MKXPZ_VERSION "/") + getPlistValue("GIT_COMMIT_HASH");
    VALUE vers = rb_utf8_str_new_cstr(version.c_str());
#else
    VALUE vers = rb_utf8_str_new_cstr(MKXPZ_VERSION "/" MKXPZ_GIT_HASH);
#endif
    rb_str_freeze(vers);
    rb_define_const(mod, "VERSION", vers);
    
    // For iOS with statically linked Ruby extensions, initialize zlib directly
    // The require mechanism doesn't work properly with static extensions on iOS
    // Note: Encoding is already initialized by ruby_init(), don't call Init_enc/Init_Encoding/Init_encodings
    Init_zlib();
    Debug() << "Zlib initialized directly for static linking";
    
    // For static linking, we must tell Ruby that the feature is already loaded
    // otherwise require 'zlib' will fail because it can't find zlib.so/zlib.rb
    // IMPORTANT: Add entries unconditionally (with duplicate check) since Init_zlib() was called
    int state;
    rb_eval_string_protect(
        // Add zlib to LOADED_FEATURES so require 'zlib' returns false instead of throwing error
        // Use include? check to avoid duplicates if this runs multiple times
        "$LOADED_FEATURES << 'zlib.so' unless $LOADED_FEATURES.include?('zlib.so'); "
        "$LOADED_FEATURES << 'zlib.rb' unless $LOADED_FEATURES.include?('zlib.rb'); "
        "$LOADED_FEATURES << 'zlib' unless $LOADED_FEATURES.include?('zlib'); "
        , &state);
    
    // Stub Win32API ONLY if the game doesn't already define it
    // Many games define their own Win32API with different inheritance, so we check first
    rb_eval_string_protect(
        "unless defined?(Win32API)\n"
        "  class Win32API\n"
        "    def initialize(dllname, func, import, export = '0', call_type = nil)\n"
        "      @dll = dllname; @func = func\n"
        "    end\n"
        "    def call(*args); return 0; end\n"
        "    def Call(*args); return 0; end\n"
        "  end\n"
        "end\n"
        // Prevent MiniFFI from crashing on system DLLs if it's used directly
        "class MiniFFI; def initialize(*args); end; def call(*args); 0; end; end unless defined?(MiniFFI)\n"
        , &state);
    
    if (state != 0) {
        Debug() << "Warning: Error during zlib/Win32API setup, continuing anyway";
    }
    
    // Define common Encoding constants if they're missing (Ruby 4.0 dev / static linking issue)
    // The Encoding class and encodings exist, but the constants may not be exposed
    // Each line has its own rescue block to continue even if one encoding fails
    rb_eval_string_protect(
        "class Encoding\n"
        "  UTF_8 = find('UTF-8') rescue nil unless const_defined?(:UTF_8) rescue nil\n"
        "  ASCII_8BIT = find('ASCII-8BIT') rescue nil unless const_defined?(:ASCII_8BIT) rescue nil\n"
        "  BINARY = ASCII_8BIT unless const_defined?(:BINARY) rescue nil\n"
        "  US_ASCII = find('US-ASCII') rescue nil unless const_defined?(:US_ASCII) rescue nil\n"
        "end\n",
        &state);
    if (state == 0) {
        Debug() << "Encoding constants defined successfully";
    } else {
        Debug() << "Warning: Could not define Encoding constants";
        // Clear the error state
        rb_errinfo();
    }
    
    // Ruby 1.8/1.9 compatibility shims for Ruby 4.0
    // Many old RPG Maker games use deprecated/removed methods
    rb_eval_string_protect(
        // File.exists? was removed in Ruby 3.2, use File.exist? instead
        "class << File\n"
        "  alias_method :exists?, :exist? unless method_defined?(:exists?)\n"
        "end\n"
        // Dir.exists? was also removed
        "class << Dir\n"
        "  alias_method :exists?, :exist? unless method_defined?(:exists?)\n"
        "end\n"
        // Object#to_a was removed in Ruby 3.0
        "class Object\n"
        "  def to_a; [self]; end unless method_defined?(:to_a)\n"
        "end\n"
        // NilClass#to_a returns empty array
        "class NilClass\n"
        "  def to_a; []; end unless method_defined?(:to_a)\n"
        "end\n"
        // Hash#to_a returns array of pairs
        "class Hash\n"
        "  alias :old_to_a :to_a unless method_defined?(:old_to_a)\n"
        "end\n"
        , &state);
    
    if (state == 0) {
        Debug() << "Ruby compatibility shims defined successfully";
    } else {
        Debug() << "Warning: Could not define Ruby compatibility shims";
        rb_errinfo();
    }
    
    // Ruby 4.0 private method visibility fix
    // In Ruby 4.0, calling a private method from outside the class raises NoMethodError
    // Older Ruby versions (1.8, 1.9) were more lenient with private method access
    // Many RPG Maker games define 'dispose' as private which worked in older Ruby
    // This patch makes common private methods accessible via a module that can be mixed in
    rb_eval_string_protect(
        // Create a module that provides public wrappers for common private methods
        "module MKXPZPrivateMethodFix\n"
        "  # Common methods that games might define as private but call publicly\n"
        "  # Core RGSS methods\n"
        "  PRIVATE_METHODS_TO_FIX = [:dispose, :disposed?, :update, :refresh, :terminate, :create, :contents,\n"
        "    # Pokemon Essentials methods (defined via module_function, which makes them private in Ruby 3+)\n"
        "    :pbShowCommands, :pbShowCommandsWithHelp, :pbMessage, :pbMessageDisplay,\n"
        "    :pbConfirmMessage, :pbConfirmMessageSerious, :pbShowCommandsBlack, :pbShowCommandsBorde]\n"
        "  \n"
        "  def self.included(base)\n"
        "    base.class_eval do\n"
        "      # Store original method_missing if it exists\n"
        "      if method_defined?(:method_missing) && !method_defined?(:__mkxpz_original_method_missing__)\n"
        "        alias_method :__mkxpz_original_method_missing__, :method_missing\n"
        "      end\n"
        "      \n"
        "      def method_missing(method_name, *args, &block)\n"
        "        # Check if this is a common private method we should forward\n"
        "        if MKXPZPrivateMethodFix::PRIVATE_METHODS_TO_FIX.include?(method_name)\n"
        "          if respond_to?(method_name, true)  # Check if private method exists\n"
        "            return send(method_name, *args, &block)\n"
        "          end\n"
        "        end\n"
        "        # Call original method_missing or raise NoMethodError\n"
        "        if respond_to?(:__mkxpz_original_method_missing__, true)\n"
        "          __mkxpz_original_method_missing__(method_name, *args, &block)\n"
        "        else\n"
        "          super\n"
        "        end\n"
        "      end\n"
        "      \n"
        "      def respond_to_missing?(method_name, include_private = false)\n"
        "        MKXPZPrivateMethodFix::PRIVATE_METHODS_TO_FIX.include?(method_name) || super\n"
        "      end\n"
        "    end\n"
        "  end\n"
        "end\n"
        "\n"
        // Apply fix to Object so all classes get it
        "class Object\n"
        "  include MKXPZPrivateMethodFix\n"
        "end\n"
        "\n"
        // Pokemon Essentials specific: Make Kernel module_function methods public
        // This is safe because we only target methods that ARE defined and ARE private
        "module Kernel\n"
        "  class << self\n"
        "    [:pbShowCommands, :pbShowCommandsWithHelp, :pbMessage, :pbMessageDisplay,\n"
        "     :pbConfirmMessage, :pbConfirmMessageSerious, :pbShowCommandsBlack, :pbShowCommandsBorde,\n"
        "     :pbChooseNumber, :pbChoosePokemon, :pbChooseNonEggPokemon, :pbChooseAblePokemon].each do |m|\n"
        "      public m if private_method_defined?(m) rescue nil\n"
        "    end\n"
        "  end\n"
        "end\n"
        , &state);
    
    if (state == 0) {
        Debug() << "Private method visibility fix installed successfully";
    } else {
        Debug() << "Warning: Could not install private method visibility fix";
        rb_errinfo();
    }
    
    // Stub RGSS Linker and other Windows-specific DLL scripts
    // Some games use: require 'RGSS Linker' which fails on iOS
    // This marks the library as loaded and patches require to handle it
    rb_eval_string_protect(
        // Mark RGSS Linker variants as already loaded
        "$LOADED_FEATURES << 'RGSS Linker' unless $LOADED_FEATURES.include?('RGSS Linker');\n"
        "$LOADED_FEATURES << 'RGSS Linker.rb' unless $LOADED_FEATURES.include?('RGSS Linker.rb');\n"
        "$LOADED_FEATURES << 'RGSS Linker.so' unless $LOADED_FEATURES.include?('RGSS Linker.so');\n"
        "$LOADED_FEATURES << 'RGSS Linker.dll' unless $LOADED_FEATURES.include?('RGSS Linker.dll');\n"
        // Create a stub RGSSLinker module
        "module RGSSLinker\n"
        "  def self.method_missing(name, *args, &block); nil; end\n"
        "  def self.respond_to_missing?(name, include_private = false); true; end\n"
        "end\n"
        // Create stub DL module (for games using Ruby's DL library for FFI)
        "module DL\n"
        "  RTLD_LAZY = 0x00001\n"
        "  RTLD_NOW  = 0x00002\n"
        "  RTLD_GLOBAL = 0x00100\n"
        "  class DLError < StandardError; end\n"
        "  class Handle\n"
        "    def initialize(lib = nil, flags = RTLD_LAZY); @lib = lib; end\n"
        "    def [](func); Pointer.new(0); end\n"
        "    def sym(func); Pointer.new(0); end\n"
        "    def close; end\n"
        "    def self.sym(lib, func); Pointer.new(0); end\n"
        "  end\n"
        "  class Pointer\n"
        "    def initialize(addr = 0); @addr = addr; end\n"
        "    def to_i; 0; end\n"
        "    def ptr; self; end\n"
        "    def ref; self; end\n"
        "    def null?; true; end\n"
        "    def to_ptr; self; end\n"
        "    def free; end\n"
        "    def +@; 0; end\n"
        "    def +(n); Pointer.new(0); end\n"
        "    def -(n); Pointer.new(0); end\n"
        "    def [](start, len = nil); ''; end\n"
        "  end\n"
        "  class Function\n"
        "    def initialize(cfunc, argtypes, abi = nil); @cfunc = cfunc; end\n"
        "    def call(*args); 0; end\n"
        "  end\n"
        "  class CFunc\n"
        "    def initialize(addr, type = 0, name = nil, calltype = nil); end\n"
        "    def call(*args); 0; end\n"
        "  end\n"
        "  def self.dlopen(lib = nil, flag = RTLD_LAZY); Handle.new(lib, flag); end\n"
        "  def self.malloc(size); Pointer.new(0); end\n"
        "  def self.free(ptr); end\n"
        "  NULL = Pointer.new(0)\n"
        "end unless defined?(DL)\n"
        // Create stub Fiddle module (newer Ruby FFI library)
        "module Fiddle\n"
        "  RTLD_LAZY = 0x00001\n"
        "  RTLD_NOW  = 0x00002\n"
        "  RTLD_GLOBAL = 0x00100\n"
        "  TYPE_VOID = 0\n"
        "  TYPE_VOIDP = 1\n"
        "  TYPE_CHAR = 2\n"
        "  TYPE_SHORT = 3\n"
        "  TYPE_INT = 4\n"
        "  TYPE_LONG = 5\n"
        "  TYPE_LONG_LONG = 6\n"
        "  TYPE_FLOAT = 7\n"
        "  TYPE_DOUBLE = 8\n"
        "  class DLError < StandardError; end\n"
        "  class Handle\n"
        "    RTLD_DEFAULT = nil\n"
        "    RTLD_NEXT = nil\n"
        "    def initialize(lib = nil, flags = RTLD_LAZY); @lib = lib; end\n"
        "    def [](func); Pointer.new(0); end\n"
        "    def sym(func); 0; end\n"
        "    def close; end\n"
        "    def self.sym(lib, func); 0; end\n"
        "  end\n"
        "  class Pointer\n"
        "    attr_accessor :free\n"
        "    def initialize(addr = 0, size = 0, freefunc = nil); @addr = addr; @size = size; end\n"
        "    def to_i; 0; end\n"
        "    def to_int; 0; end\n"
        "    def ptr; self; end\n"
        "    def ref; self; end\n"
        "    def null?; true; end\n"
        "    def to_s(len = nil); ''; end\n"
        "    def to_str(len = nil); ''; end\n"
        "    def [](start, len = nil); ''; end\n"
        "    def []=(start, len = nil, val = nil); end\n"
        "    def size; 0; end\n"
        "    def size=(s); end\n"
        "    def +(n); Pointer.new(0); end\n"
        "    def -(n); Pointer.new(0); end\n"
        "    def self.to_ptr(val); Pointer.new(0); end\n"
        "    def self.[](val); Pointer.new(0); end\n"
        "    def self.malloc(size, free = nil); Pointer.new(0, size); end\n"
        "  end\n"
        "  class Function < Pointer\n"
        "    DEFAULT = 0\n"
        "    STDCALL = 1\n"
        "    def initialize(ptr, args, ret_type, abi = DEFAULT, name: nil); super(0); end\n"
        "    def call(*args); 0; end\n"
        "  end\n"
        "  class Closure\n"
        "    def initialize(ret, args, abi = Function::DEFAULT); end\n"
        "    def call(*args); 0; end\n"
        "    def to_i; 0; end\n"
        "  end\n"
        "  def self.dlopen(lib = nil, flag = RTLD_LAZY); Handle.new(lib, flag); end\n"
        "  def self.malloc(size); Pointer.new(0, size); end\n"
        "  def self.free(ptr); end\n"
        "  def self.dlwrap(val); 0; end\n"
        "  def self.dlunwrap(addr); nil; end\n"
        "  NULL = Pointer.new(0)\n"
        "end unless defined?(Fiddle)\n"
        // Patch Kernel.require AND Kernel.load to handle Windows DLL patterns
        // Also add load_module stub for FMODEX and other audio scripts
        "module Kernel\n"
        "  alias :__mkxpz_original_require :require unless method_defined?(:__mkxpz_original_require)\n"
        "  alias :__mkxpz_original_load :load unless method_defined?(:__mkxpz_original_load)\n"
        "  def require(name)\n"
        "    name_lower = name.to_s.downcase\n"
        "    # Silently succeed for Windows-specific libraries\n"
        "    return false if name_lower.include?('rgss linker') || name_lower.include?('rgss_linker')\n"
        "    return false if name_lower.include?('rubyscreen')\n"
        "    return false if name_lower.end_with?('.dll')\n"
        "    return false if name_lower.include?('dl') && name_lower.length <= 4\n"
        "    return false if name_lower == 'win32api'\n"
        "    return false if name_lower.include?('fmod') || name_lower.include?('bass')\n"
        "    __mkxpz_original_require(name)\n"
        "  end\n"
        "  def load(name, wrap = false)\n"
        "    name_lower = name.to_s.downcase\n"
        "    # Silently succeed for Windows DLLs\n"
        "    return true if name_lower.end_with?('.dll')\n"
        "    return true if name_lower.include?('rgss linker') || name_lower.include?('rgss_linker')\n"
        "    __mkxpz_original_load(name, wrap)\n"
        "  end\n"
        "  # Stub for Windows DLL loading used by FMODEX and similar audio scripts\n"
        "  def load_module(*args); nil; end\n"
        "  module_function :load_module\n"
        "end\n",
        &state);
    
    if (state == 0) {
        Debug() << "RGSS Linker stub and require patch installed successfully";
    } else {
        Debug() << "Warning: Could not install RGSS Linker stub";
        rb_errinfo();
    }
    
    // FmodEx stub module for FMODEX audio scripts - separate eval to avoid GC issues
    rb_eval_string_protect(
        "module FmodEx\n"
        "  FMOD_OK = 0\n"
        "  FMOD_ERR_INVALID_HANDLE = 36\n"
        "  FMOD_DEFAULT = 0\n"
        "  FMOD_LOOP_OFF = 1\n"
        "  FMOD_LOOP_NORMAL = 2\n"
        "  FMOD_2D = 8\n"
        "  FMOD_3D = 16\n"
        "  FMOD_SOFTWARE = 64\n"
        "  FMOD_HARDWARE = 32\n"
        "  FMOD_CREATESTREAM = 128\n"
        "  FMOD_CREATESAMPLE = 256\n"
        "  class System\n"
        "    def initialize; end\n"
        "    def init(*a); 0; end\n"
        "    def close; 0; end\n"
        "    def release; 0; end\n"
        "    def update; 0; end\n"
        "    def createSound(*a); [0, Sound.new]; end\n"
        "    def createStream(*a); [0, Sound.new]; end\n"
        "    def playSound(*a); [0, Channel.new]; end\n"
        "    def getChannel(*a); [0, Channel.new]; end\n"
        "    def method_missing(m, *a); 0; end\n"
        "  end\n"
        "  class Sound\n"
        "    def initialize; end\n"
        "    def release; 0; end\n"
        "    def getLength(*a); [0, 0]; end\n"
        "    def method_missing(m, *a); 0; end\n"
        "  end\n"
        "  class Channel\n"
        "    def initialize; end\n"
        "    def stop; 0; end\n"
        "    def setPaused(p); 0; end\n"
        "    def getPaused(*a); [0, false]; end\n"
        "    def setVolume(v); 0; end\n"
        "    def getVolume(*a); [0, 1.0]; end\n"
        "    def isPlaying(*a); [0, false]; end\n"
        "    def method_missing(m, *a); 0; end\n"
        "  end\n"
        "  class ChannelGroup\n"
        "    def initialize; end\n"
        "    def method_missing(m, *a); 0; end\n"
        "  end\n"
        "  def self.System_Create(*a); [0, System.new]; end\n"
        "  def self.method_missing(m, *a); 0; end\n"
        "end unless defined?(FmodEx)\n",
        &state);
    
    if (state == 0) {
        Debug() << "FmodEx stub installed successfully";
    } else {
        Debug() << "Warning: Could not install FmodEx stub";
        rb_errinfo();
    }
    
    // BASS audio library stub - separate eval
    rb_eval_string_protect(
        "module BASS\n"
        "  def self.method_missing(m, *a); 0; end\n"
        "end unless defined?(BASS)\n",
        &state);
    
    if (state == 0) {
        Debug() << "BASS stub installed successfully";
        fprintf(stderr, "[MKXP-Z] DEBUG: mriBindingInit completed successfully\n");
    } else {
        Debug() << "Warning: Could not install BASS stub";
        fprintf(stderr, "[MKXP-Z] DEBUG: mriBindingInit completed with warnings\n");
        rb_errinfo();
    }
    
    // =============================================================================
    // NOTE: The pbShowCommands private method issue in RPG Maker Essentials games
    // cannot be fixed safely at the MKXP-Z level without breaking other games.
    // Previous attempts to patch Module#module_function, Kernel.private, or 
    // Object#method_missing all caused regressions in other games.
    // 
    // This is a Ruby 4.0 strictness issue where module_function makes methods
    // private, and calling them from outside raises NoMethodError. Games that
    // depend on this behavior need to be patched at the game script level by
    // adding `public :pbShowCommands` after the module_function declaration.
    // =============================================================================
    
    // =============================================================================
    // LEGACY RUBY 1.8/1.9 COMPATIBILITY SHIMS FOR OLD RPG MAKER GAMES
    // These are only applied for rgssVer <= 2 (RPG Maker XP and VX) to avoid
    // affecting newer games that might rely on modern Ruby behavior.
    // =============================================================================
    if (rgssVer <= 2) {
        fprintf(stderr, "[MKXP-Z] INFO: Applying legacy Ruby 1.8/1.9 compatibility shims for RGSS%d\n", rgssVer);
        
        // Fixnum and Bignum were unified into Integer in Ruby 2.4
        // Many old scripts check for these classes directly
        rb_eval_string_protect(
            "unless defined?(Fixnum)\n"
            "  Fixnum = Integer\n"
            "end\n"
            "unless defined?(Bignum)\n"
            "  Bignum = Integer\n"
            "end\n",
            &state);
        
        if (state == 0) {
            Debug() << "Fixnum/Bignum compatibility shims installed";
        } else {
            Debug() << "Warning: Could not install Fixnum/Bignum shims";
            rb_errinfo();
        }
        
        // Object#id was deprecated in Ruby 1.9, removed in Ruby 2.x
        // Some old games use it instead of object_id
        rb_eval_string_protect(
            "class Object\n"
            "  alias_method :id, :object_id unless method_defined?(:id) rescue nil\n"
            "end\n",
            &state);
        
        // String#to_a was removed in Ruby 1.9, used to return [self]
        rb_eval_string_protect(
            "class String\n"
            "  def to_a\n"
            "    [self]\n"
            "  end unless method_defined?(:to_a)\n"
            "end\n",
            &state);
        
        // Hash#index was renamed to Hash#key in Ruby 1.9
        rb_eval_string_protect(
            "class Hash\n"
            "  alias_method :index, :key unless method_defined?(:index) rescue nil\n"
            "end\n",
            &state);
        
        // Thread.critical was removed in Ruby 2.0
        // This was used for basic thread synchronization
        rb_eval_string_protect(
            "class << Thread\n"
            "  def critical; false; end unless method_defined?(:critical)\n"
            "  def critical=(val); end unless method_defined?(:critical=)\n"
            "end\n",
            &state);
        
        // NOTE: The following shims were REMOVED because they caused crashes:
        // - Symbol#to_int - could interfere with symbol comparisons
        // - Struct#members - incomplete and unnecessary  
        // - Proc#call arity override - very risky, changed core behavior
        // - respond_to? override - risky, changed core behavior
        // These shims caused memzero/bad_access crashes in some XP games.
        // Only keeping safe, additive shims that don't override core methods.
        
        // NOTE: $SAFE was removed in Ruby 2.7, but we cannot stub it because
        // global variables cannot be defined as methods. Games that use $SAFE
        // directly will need to be patched. Most games don't use this feature.
        
        // Array#nitems was removed in Ruby 1.9, counts non-nil elements
        rb_eval_string_protect(
            "class Array\n"
            "  def nitems\n"
            "    count { |item| !item.nil? }\n"
            "  end unless method_defined?(:nitems)\n"
            "end\n",
            &state);
        
        // String#each was removed in Ruby 1.9 (use each_line instead)
        rb_eval_string_protect(
            "class String\n"
            "  alias_method :each, :each_line unless method_defined?(:each)\n"
            "end\n",
            &state);
        
        fprintf(stderr, "[MKXP-Z] INFO: Legacy Ruby compatibility shims installed for RGSS%d\n", rgssVer);
    } else {
        fprintf(stderr, "[MKXP-Z] DEBUG: Skipping legacy shims for RGSS%d (not needed for VX Ace)\n", rgssVer);
    }
    
    // Set $stdout and its ilk accordingly on Windows
    // I regret teaching you that word
#ifdef __WIN32__
    if (shState->config().winConsole)
        configureWindowsStreams();
#endif
}

static void showMsg(const std::string &msg) {
    shState->eThread().showMessageBox(msg.c_str());
}

static void printP(int argc, VALUE *argv, const char *convMethod,
                   const char *sep) {
    VALUE dispString = rb_str_buf_new(128);
    ID conv = rb_intern(convMethod);
    
    for (int i = 0; i < argc; ++i) {
        VALUE str = rb_funcall2(argv[i], conv, 0, NULL);
        rb_str_buf_append(dispString, str);
        
        if (i < argc)
            rb_str_buf_cat2(dispString, sep);
    }
    
    showMsg(RSTRING_PTR(dispString));
}


RB_METHOD_GUARD(mriPrint) {
    RB_UNUSED_PARAM;
    
    printP(argc, argv, "to_s", "");
    
    shState->checkShutdown();
    shState->checkReset();
    
    return Qnil;
}
RB_METHOD_GUARD_END

RB_METHOD_GUARD(mriP) {
    RB_UNUSED_PARAM;
    
    printP(argc, argv, "inspect", "\n");
    
    shState->checkShutdown();
    shState->checkReset();
    
    return Qnil;
}
RB_METHOD_GUARD_END

RB_METHOD(mkxpDelta) {
    RB_UNUSED_PARAM;
    return rb_float_new(shState->runTime());
}

RB_METHOD(mkxpDataDirectory) {
    RB_UNUSED_PARAM;
    
    const std::string &path = shState->config().customDataPath;
    const char *s = path.empty() ? "." : path.c_str();
    
    std::string s_nml = shState->fileSystem().normalize(s, 1, 1);
    VALUE ret = rb_utf8_str_new_cstr(s_nml.c_str());
    
    return ret;
}

RB_METHOD(mkxpSetTitle) {
    RB_UNUSED_PARAM;
    
    VALUE s;
    rb_scan_args(argc, argv, "1", &s);
    SafeStringValue(s);
    
    shState->eThread().requestWindowRename(RSTRING_PTR(s));
    return s;
}

RB_METHOD(mkxpGetTitle) {
    RB_UNUSED_PARAM;
    
    rb_check_argc(argc, 0);
    
    return rb_utf8_str_new_cstr(SDL_GetWindowTitle(shState->sdlWindow()));
}

RB_METHOD(mkxpDesensitize) {
    RB_UNUSED_PARAM;
    
    VALUE filename;
    rb_scan_args(argc, argv, "1", &filename);
    SafeStringValue(filename);
    
    return rb_utf8_str_new_cstr(
                                shState->fileSystem().desensitize(RSTRING_PTR(filename)));
}

RB_METHOD(mkxpPuts) {
    RB_UNUSED_PARAM;
    
    const char *str;
    rb_get_args(argc, argv, "z", &str RB_ARG_END);
    
    Debug() << str;
    
    return Qnil;
}

RB_METHOD(mkxpPlatform) {
    RB_UNUSED_PARAM;
    
#if MKXPZ_PLATFORM == MKXPZ_PLATFORM_MACOS
    std::string platform("macOS");
    
    if (mkxp_sys::isRosetta())
        platform += " (Rosetta)";
    
#elif MKXPZ_PLATFORM == MKXPZ_PLATFORM_WINDOWS
    std::string platform("Windows");
    
    if (mkxp_sys::isWine()) {
        platform += " (Wine - ";
        switch (mkxp_sys::getRealHostType()) {
            case mkxp_sys::WineHostType::Mac:
                platform += "macOS)";
                break;
            default:
                platform += "Linux)";
                break;
        }
    }
#else
    std::string platform("Linux");
#endif
    
    return rb_utf8_str_new_cstr(platform.c_str());
}

RB_METHOD(mkxpIsMacHost) {
    RB_UNUSED_PARAM;
    
    return rb_bool_new(MKXPZ_PLATFORM == MKXPZ_PLATFORM_MACOS);
}

RB_METHOD(mkxpIsUsingRosetta) {
    RB_UNUSED_PARAM;
    
    return rb_bool_new(mkxp_sys::isRosetta());
}

RB_METHOD(mkxpIsLinuxHost) {
    RB_UNUSED_PARAM;
    
    return rb_bool_new(MKXPZ_PLATFORM == MKXPZ_PLATFORM_LINUX);
}

RB_METHOD(mkxpIsWindowsHost) {
    RB_UNUSED_PARAM;
    
    return rb_bool_new(MKXPZ_PLATFORM == MKXPZ_PLATFORM_WINDOWS);
}

RB_METHOD(mkxpIsUsingWine) {
    RB_UNUSED_PARAM;
    return rb_bool_new(mkxp_sys::isWine());
}

RB_METHOD(mkxpIsReallyMacHost) {
    RB_UNUSED_PARAM;
    return rb_bool_new(mkxp_sys::getRealHostType() == mkxp_sys::WineHostType::Mac);
}

RB_METHOD(mkxpIsReallyLinuxHost) {
    RB_UNUSED_PARAM;
    return rb_bool_new(mkxp_sys::getRealHostType() == mkxp_sys::WineHostType::Linux);
}

RB_METHOD(mkxpIsReallyWindowsHost) {
    RB_UNUSED_PARAM;
    return rb_bool_new(mkxp_sys::getRealHostType() == mkxp_sys::WineHostType::Windows);
}

RB_METHOD(mkxpUserLanguage) {
    RB_UNUSED_PARAM;
    
    return rb_utf8_str_new_cstr(mkxp_sys::getSystemLanguage().c_str());
}

RB_METHOD(mkxpUserName) {
    RB_UNUSED_PARAM;
    
    // Using the Windows API isn't working with usernames that involve Unicode
    // characters for some dumb reason
#ifdef __WIN32__
    VALUE env = rb_const_get(rb_mKernel, rb_intern("ENV"));
    return rb_funcall(env, rb_intern("[]"), 1, rb_str_new_cstr("USERNAME"));
#else
    return rb_utf8_str_new_cstr(mkxp_sys::getUserName().c_str());
#endif
}

RB_METHOD(mkxpGameTitle) {
    RB_UNUSED_PARAM;
    
    return rb_utf8_str_new_cstr(shState->config().game.title.c_str());
}

RB_METHOD(mkxpPowerState) {
    RB_UNUSED_PARAM;
    
    int secs, pct;
    SDL_PowerState ps = SDL_GetPowerInfo(&secs, &pct);
    
    VALUE hash = rb_hash_new();
    
    rb_hash_aset(hash, ID2SYM(rb_intern("seconds")),
                 (secs > -1) ? INT2NUM(secs) : RUBY_Qnil);
    
    rb_hash_aset(hash, ID2SYM(rb_intern("percent")),
                 (pct > -1) ? INT2NUM(pct) : RUBY_Qnil);
    
    rb_hash_aset(hash, ID2SYM(rb_intern("discharging")),
                 rb_bool_new(ps == SDL_POWERSTATE_ON_BATTERY));
    
    return hash;
}

RB_METHOD(mkxpSettingsMenu) {
    RB_UNUSED_PARAM;
    
    shState->eThread().requestSettingsMenu();
    
    return Qnil;
}

RB_METHOD(mkxpCpuCount) {
    RB_UNUSED_PARAM;
    
    return INT2NUM(SDL_GetCPUCount());
}

RB_METHOD(mkxpSystemMemory) {
    RB_UNUSED_PARAM;
    
    return INT2NUM(SDL_GetSystemRAM());
}

RB_METHOD_GUARD(mkxpReloadPathCache) {
    RB_UNUSED_PARAM;
    
    shState->fileSystem().reloadPathCache();
    return Qnil;
}
RB_METHOD_GUARD_END

RB_METHOD_GUARD(mkxpAddPath) {
    RB_UNUSED_PARAM;
    
    VALUE path, mountpoint, reload;
    rb_scan_args(argc, argv, "12", &path, &mountpoint, &reload);
    SafeStringValue(path);
    if (mountpoint != Qnil) SafeStringValue(mountpoint);
    
    const char *mp = (mountpoint == Qnil) ? 0 : RSTRING_PTR(mountpoint);
    
    bool rl = true;
    if (reload != Qnil)
        rb_bool_arg(reload, &rl);
    
    shState->fileSystem().addPath(RSTRING_PTR(path), mp, rl);
    
    return path;
}
RB_METHOD_GUARD_END

RB_METHOD_GUARD(mkxpRemovePath) {
    RB_UNUSED_PARAM;
    
    VALUE path, reload;
    rb_scan_args(argc, argv, "11", &path, &reload);
    SafeStringValue(path);
    
    bool rl = true;
    if (reload != Qnil)
        rb_bool_arg(reload, &rl);
    
    shState->fileSystem().removePath(RSTRING_PTR(path), rl);
    
    return path;
}
RB_METHOD_GUARD_END

RB_METHOD(mkxpFileExists) {
    RB_UNUSED_PARAM;
    
    VALUE path;
    rb_scan_args(argc, argv, "1", &path);
    SafeStringValue(path);
    
    if (shState->fileSystem().exists(RSTRING_PTR(path)))
        return Qtrue;
    return Qfalse;
}

RB_METHOD(mkxpSetDefaultFontFamily) {
    RB_UNUSED_PARAM;
    
    VALUE familyV;
    rb_scan_args(argc, argv, "1", &familyV);
    SafeStringValue(familyV);
    
    std::string family(RSTRING_PTR(familyV));
    shState->fontState().setDefaultFontFamily(family);
    
    return Qnil;
}

RB_METHOD_GUARD(mkxpStringToUTF8) {
    RB_UNUSED_PARAM;
    
    rb_check_argc(argc, 0);
    
    std::string ret(RSTRING_PTR(self), RSTRING_LEN(self));
    ret = Encoding::convertString(ret);
    
    return rb_utf8_str_new(ret.c_str(), ret.length());
}
RB_METHOD_GUARD_END

RB_METHOD_GUARD(mkxpStringToUTF8Bang) {
    RB_UNUSED_PARAM;
    
    rb_check_argc(argc, 0);
    
    std::string ret(RSTRING_PTR(self), RSTRING_LEN(self));
    ret = Encoding::convertString(ret);
    
    rb_str_resize(self, ret.length());
    memcpy(RSTRING_PTR(self), ret.c_str(), RSTRING_LEN(self));
    
#if RAPI_FULL >= 190
    rb_funcall(self, rb_intern("force_encoding"), 1, rb_enc_from_encoding(rb_utf8_encoding()));
#endif
    
    return self;
}
RB_METHOD_GUARD_END

#ifdef __APPLE__
#define OPENCMD "open "
#define OPENARGS "--args"
#elif defined(__linux__)
#define OPENCMD "xdg-open "
#define OPENARGS ""
#else
#define OPENCMD "start /b \"launch\" "
#define OPENARGS ""
#endif

RB_METHOD_GUARD(mkxpLaunch) {
    RB_UNUSED_PARAM;
    
    VALUE cmdname, args;
    
    rb_scan_args(argc, argv, "11", &cmdname, &args);
    SafeStringValue(cmdname);
    
    std::string command(OPENCMD);
    command += "\""; command += RSTRING_PTR(cmdname); command += "\"";
    
    if (args != RUBY_Qnil) {
#ifndef __linux__
        command += " ";
        command += OPENARGS;
        Check_Type(args, T_ARRAY);
        
        for (int i = 0; i < RARRAY_LEN(args); i++) {
            VALUE arg = rb_ary_entry(args, i);
            SafeStringValue(arg);
            
            if (RSTRING_LEN(arg) <= 0)
                continue;
            
            command += " ";
            command += RSTRING_PTR(arg);
        }
#else
        Debug() << command << ":" << "Arguments are not supported with xdg-open. Ignoring.";
#endif
    }
    
    if ((throw Exception(Exception::MKXPError, "launch not supported on iOS"), -1) != 0) {
        throw Exception(Exception::MKXPError, "Failed to launch \"%s\"", RSTRING_PTR(cmdname));
    }
    
    return RUBY_Qnil;
}
RB_METHOD_GUARD_END

RB_METHOD_GUARD(mkxpParseCSV) {
    RB_UNUSED_PARAM;
    
    VALUE str;
    rb_scan_args(argc, argv, "1", &str);
    SafeStringValue(str);
    
    VALUE ret = rb_ary_new();
    std::stringstream stream(RSTRING_PTR(str));
    try {
        rapidcsv::Document doc(stream, rapidcsv::LabelParams(-1,-1), rapidcsv::SeparatorParams(',', false, true, true, true));
        for (int r = 0; r < doc.GetRowCount(); r++) {
            VALUE col = rb_ary_new();
            for (int c = 0; c < doc.GetColumnCount(); c++) {
                std::string str = doc.GetCell<std::string>(c, r);
                rb_ary_push(col, rb_utf8_str_new(str.c_str(), str.length()));
            }
            rb_ary_push(ret, col);
        }
    } catch (std::exception &e) {
        throw Exception(Exception::MKXPError, "Failed to parse CSV: %s", e.what());
    }

    return ret;
}
RB_METHOD_GUARD_END

json5pp::value loadUserSettings() {
    json5pp::value ret;
    VALUE cpath = rb_utf8_str_new_cstr(shState->config().userConfPath.c_str());
    
    if (rb_funcall(rb_cFile, rb_intern("exists?"), 1, cpath) == Qtrue) {
        VALUE f = rb_funcall(rb_cFile, rb_intern("open"), 2, cpath, rb_str_new("r", 1));
        VALUE data = rb_funcall(f, rb_intern("read"), 0);
        rb_funcall(f, rb_intern("close"), 0);
        ret = json5pp::parse5(RSTRING_PTR(data));
    }
    
    if (!ret.is_object())
        ret = json5pp::object({});
    
    return ret;
}

void saveUserSettings(json5pp::value &settings) {
    VALUE cpath = rb_utf8_str_new_cstr(shState->config().userConfPath.c_str());
    VALUE f = rb_funcall(rb_cFile, rb_intern("open"), 2, cpath, rb_str_new("w", 1));
    rb_funcall(f, rb_intern("write"), 1, rb_utf8_str_new_cstr(settings.stringify5(json5pp::rule::space_indent<>()).c_str()));
    rb_funcall(f, rb_intern("close"), 0);
}

RB_METHOD(mkxpGetJSONSetting) {
    RB_UNUSED_PARAM;
    
    VALUE sname;
    rb_scan_args(argc, argv, "1", &sname);
    SafeStringValue(sname);
    
    auto settings = loadUserSettings();
    auto &s = settings.as_object();
    
    if (s[RSTRING_PTR(sname)].is_null()) {
        return json2rb(shState->config().raw.as_object()[RSTRING_PTR(sname)]);
    }
    
    return json2rb(s[RSTRING_PTR(sname)]);
    
}

RB_METHOD_GUARD(mkxpSetJSONSetting) {
    RB_UNUSED_PARAM;
    
    VALUE sname, svalue;
    rb_scan_args(argc, argv, "2", &sname, &svalue);
    SafeStringValue(sname);
    
    auto settings = loadUserSettings();
    auto &s = settings.as_object();
    s[RSTRING_PTR(sname)] = rb2json(svalue);
    saveUserSettings(settings);
    
    return Qnil;
}
RB_METHOD_GUARD_END

RB_METHOD(mkxpGetAllJSONSettings) {
    RB_UNUSED_PARAM;
    
    return json2rb(shState->config().raw);
}

static VALUE rgssMainCb(VALUE block) {
    rb_funcall2(block, rb_intern("call"), 0, 0);
    return Qnil;
}

static VALUE rgssMainRescue(VALUE arg, VALUE exc) {
    VALUE *excRet = (VALUE *)arg;
    
    *excRet = exc;
    
    return Qnil;
}

static bool processReset(bool rubyExc) {
	const char *str = "Audio.__reset__; Graphics.__reset__;";
	
	if (rubyExc) {
		rb_eval_string(str);
	} else {
		int state;
		rb_eval_string_protect(str, &state);
		return state;
	}
	
	return 0;
}

#if RAPI_FULL > 187
static VALUE newStringUTF8(const char *string, long length) {
    return rb_enc_str_new(string, length, rb_utf8_encoding());
}
#else
#define newStringUTF8 rb_str_new
#endif

struct evalArg {
    VALUE string;
    VALUE filename;
};

static VALUE evalHelper(evalArg *arg) {
    VALUE argv[] = {arg->string, Qnil, arg->filename};
    return rb_funcall2(topSelf, rb_intern("eval"), ARRAY_SIZE(argv), argv);
}

static VALUE evalString(VALUE string, VALUE filename, int *state) {
    evalArg arg = {string, filename};
    return rb_protect((VALUE(*)(VALUE))evalHelper, (VALUE)&arg, state);
}

static void runCustomScript(const std::string &filename) {
    std::string scriptData;
    
    if (!readFileSDL(filename.c_str(), scriptData)) {
        showMsg(std::string("Unable to open '") + filename + "'");
        return;
    }
    
    evalString(newStringUTF8(scriptData.c_str(), scriptData.size()),
               newStringUTF8(filename.c_str(), filename.size()), NULL);
}

RB_METHOD_GUARD(mriRgssMain) {
    RB_UNUSED_PARAM;

    /* Execute postload scripts */
    const Config &conf = shState->rtData().config;
    for (std::vector<std::string>::const_iterator i = conf.postloadScripts.begin();
        i != conf.postloadScripts.end(); ++i)
    {
        if (shState->rtData().rqTerm)
            break;
        runCustomScript(*i);
    }

    while (true) {
        VALUE exc = Qnil;
#if RAPI_FULL < 270
        rb_rescue2((VALUE(*)(ANYARGS))rgssMainCb, rb_block_proc(),
                   (VALUE(*)(ANYARGS))rgssMainRescue, (VALUE)&exc, rb_eException,
                   (VALUE)0);
#else
        rb_rescue2(rgssMainCb, rb_block_proc(), rgssMainRescue, (VALUE)&exc,
                   rb_eException, (VALUE)0);
#endif
        
        if (NIL_P(exc))
            break;
        
        if (rb_obj_class(exc) == getRbData()->exc[Reset])
            processReset(true);
        else
            rb_exc_raise(exc);
    }
    
    return Qnil;
}
RB_METHOD_GUARD_END

RB_METHOD_GUARD(mriRgssStop) {
    RB_UNUSED_PARAM;
    
    while (true)
        shState->graphics().update();
    
    return Qnil;
}
RB_METHOD_GUARD_END

RB_METHOD(_kernelCaller) {
    RB_UNUSED_PARAM;
    
    VALUE trace =
    rb_funcall2(rb_mKernel, rb_intern("_mkxp_kernel_caller_alias"), 0, 0);
    
    if (!RB_TYPE_P(trace, RUBY_T_ARRAY))
        return trace;
    
    long len = RARRAY_LEN(trace);
    
    if (len < 2)
        return trace;
    
    /* Remove useless "ruby:1:in 'eval'" */
    rb_ary_pop(trace);
    
    /* Also remove trace of this helper function */
    rb_ary_shift(trace);
    
    len -= 2;
    
    if (len == 0)
        return trace;
    
    /* RMXP does this, not sure if specific or 1.8 related */
    VALUE args[] = {rb_utf8_str_new_cstr(":in `<main>'"), rb_utf8_str_new_cstr("")};
    rb_funcall2(rb_ary_entry(trace, len - 1), rb_intern("gsub!"), 2, args);
    
    return trace;
}

VALUE kernelLoadDataInt(const char *filename, bool rubyExc, bool raw);

struct BacktraceData {
    /* Maps: Ruby visible filename, To: Actual script name */
    BoostHash<std::string, std::string> scriptNames;
};

/**
 * Preprocess Ruby 1.8/1.9 syntax to make it compatible with Ruby 3.x/4.x
 * 
 * Ruby 4.0 uses the Prism parser which doesn't support legacy Ruby 1.8 syntax.
 * This function converts:
 * - "when X:" to "when X then" (case statement shorthand)
 * - "when X, Y:" to "when X, Y then" (case with multiple values)
 * 
 * Note: This is a best-effort conversion. Complex edge cases might not be handled.
 */
static std::string preprocessRuby18Syntax(const std::string& script) {
    if (script.empty()) return script;
    
    std::string result = script;
    
    // Pattern explanation:
    // (when\s+)  - Match "when" followed by whitespace (capture group 1)
    // ([^:\n]+?) - Match the when expression(s), non-greedy (capture group 2)
    // (\s*):     - Match optional whitespace followed by colon
    // (\s*)      - Match trailing whitespace (capture group 3)
    // (?=\s*\n|\s*#|\s+[^\s:]) - Lookahead: followed by newline, comment, or next statement
    //                            (but NOT another colon, to avoid matching "a ? b : c")
    
    // Simpler, safer approach: only match "when <expr>:" at end of line or before comment
    // This avoids breaking ternary operators and hash literals
    try {
        // Match: "when" + space + expression(s) + ":" at end of line or before comment
        // The expression can contain commas (for multiple values) but not newlines
        std::regex whenColonPattern(
            R"((when\s+)([^:\n]+?)\s*:\s*(#.*)?$)",
            std::regex::multiline
        );
        
        result = std::regex_replace(result, whenColonPattern, "$1$2 then $3");
        
    } catch (const std::regex_error& e) {
        // If regex fails, just return original script
        Debug() << "Ruby 1.8 syntax preprocessor regex error:" << e.what();
        return script;
    }
    
    return result;
}

bool evalScript(VALUE string, const char *filename)
{
    int state;
    evalString(string, rb_utf8_str_new_cstr(filename), &state);
    if (state) return false;
    return true;
}


#define SCRIPT_SECTION_FMT (rgssVer >= 3 ? "{%04ld}" : "Section%03ld")

static void runRMXPScripts(BacktraceData &btData) {
    fprintf(stderr, "[MKXP-Z] DEBUG: runRMXPScripts starting...\n");
    const Config &conf = shState->rtData().config;
    const std::string &scriptPack = conf.game.scripts;
    fprintf(stderr, "[MKXP-Z] DEBUG: Script pack path: %s\n", scriptPack.c_str());
    
    if (scriptPack.empty()) {
        fprintf(stderr, "[MKXP-Z] ERROR: No script file specified!\n");
        showMsg("No script file has been specified. Check the game's INI and try again.");
        return;
    }
    
    if (!shState->fileSystem().exists(scriptPack.c_str())) {
        fprintf(stderr, "[MKXP-Z] ERROR: Script pack not found: %s\n", scriptPack.c_str());
        showMsg("Unable to load scripts from '" + scriptPack + "'");
        return;
    }
    
    VALUE scriptArray;
    
    /* We checked if Scripts.rxdata exists, but something might
     * still go wrong */
    try {
        scriptArray = kernelLoadDataInt(scriptPack.c_str(), false, false);
    } catch (const Exception &e) {
        showMsg(std::string("Failed to read script data: ") + e.msg);
        return;
    }
    
    if (!RB_TYPE_P(scriptArray, RUBY_T_ARRAY)) {
        showMsg("Failed to read script data");
        return;
    }
    
    rb_gv_set("$RGSS_SCRIPTS", scriptArray);
    
    long scriptCount = RARRAY_LEN(scriptArray);
    fprintf(stderr, "[MKXP-Z] DEBUG: Loaded %ld scripts from archive\n", scriptCount);
    
    std::string decodeBuffer;
    // Increased from 0x1000 (4KB) to 0x300000 (3MB) to prevent thread crashes
    // when decompressing large script files. Older RPG Maker games often have
    // Scripts.rxdata files that decompress to very large scripts.
    decodeBuffer.resize(0x300000);
    size_t initialBufferSize = decodeBuffer.size();
    fprintf(stderr, "[MKXP-Z] DEBUG: Initial decode buffer size: %zu bytes (%.1f MB)\n", 
            initialBufferSize, initialBufferSize / (1024.0 * 1024.0));
    
    for (long i = 0; i < scriptCount; ++i) {
        VALUE script = rb_ary_entry(scriptArray, i);
        
        if (!RB_TYPE_P(script, RUBY_T_ARRAY))
            continue;
        
        VALUE scriptName = rb_ary_entry(script, 1);
        VALUE scriptString = rb_ary_entry(script, 2);
        
        int result = Z_OK;
        unsigned long bufferLen;
        
        while (true) {
            unsigned char *bufferPtr = reinterpret_cast<unsigned char *>(
                                                                         const_cast<char *>(decodeBuffer.c_str()));
            const unsigned char *sourcePtr =
            reinterpret_cast<const unsigned char *>(RSTRING_PTR(scriptString));
            
            bufferLen = decodeBuffer.length();
            
            result = uncompress(bufferPtr, &bufferLen, sourcePtr,
                                RSTRING_LEN(scriptString));
            
            bufferPtr[bufferLen] = '\0';
            
            if (result != Z_BUF_ERROR)
                break;
            
            // Buffer too small, need to grow it
            size_t oldSize = decodeBuffer.size();
            decodeBuffer.resize(decodeBuffer.size() * 2);
            fprintf(stderr, "[MKXP-Z] WARN: Script %ld (%s) needs larger buffer: %zu -> %zu bytes\n",
                    i, RSTRING_PTR(scriptName), oldSize, decodeBuffer.size());
        }
        
        if (result != Z_OK) {
            static char buffer[256];
            snprintf(buffer, sizeof(buffer), "Error decoding script %ld: '%s'", i,
                     RSTRING_PTR(scriptName));
            
            showMsg(buffer);
            
            break;
        }
        
        // Preprocess Ruby 1.8 syntax for compatibility with Ruby 4.0's Prism parser
        std::string processedScript = preprocessRuby18Syntax(decodeBuffer.c_str());
        rb_ary_store(script, 3, rb_utf8_str_new_cstr(processedScript.c_str()));
    }
    
    /* Execute preloaded scripts */
    for (std::vector<std::string>::const_iterator i = conf.preloadScripts.begin();
         i != conf.preloadScripts.end(); ++i)
    {
        if (shState->rtData().rqTerm)
            break;
        runCustomScript(*i);
    }
    
    VALUE exc = rb_gv_get("$!");
    if (exc != Qnil)
        return;
    
    fprintf(stderr, "[MKXP-Z] DEBUG: Starting main script execution loop...\n");
    while (true) {
        for (long i = 0; i < scriptCount; ++i) {
            if (shState->rtData().rqTerm) {
                fprintf(stderr, "[MKXP-Z] DEBUG: Termination requested at script %ld\n", i);
                break;
            }
            
            VALUE script = rb_ary_entry(scriptArray, i);
            VALUE scriptDecoded = rb_ary_entry(script, 3);
            VALUE string =
            newStringUTF8(RSTRING_PTR(scriptDecoded), RSTRING_LEN(scriptDecoded));
            
            VALUE fname;
            const char *scriptName = RSTRING_PTR(rb_ary_entry(script, 1));
            char buf[512];
            int len;
            
            // iOS: Skip scripts with Windows-specific names that would fail
            // These scripts typically rely on DLLs or Windows APIs
            {
                std::string name(scriptName);
                std::string nameLower = name;
                std::transform(nameLower.begin(), nameLower.end(), nameLower.begin(), ::tolower);
                
                // Skip RGSS Linker and similar Windows-specific scripts
                if (nameLower.find("rgss linker") != std::string::npos ||
                    nameLower.find("rgss_linker") != std::string::npos ||
                    nameLower.find("rgsslinker") != std::string::npos ||
                    (nameLower.find("linker") != std::string::npos && nameLower.find("rgss") != std::string::npos)) {
                    Debug() << "[iOS] Skipping Windows-specific script: " << scriptName;
                    continue;
                }
            }
            
            if (conf.useScriptNames)
                len = snprintf(buf, sizeof(buf), "%03ld:%s", i, scriptName);
            else
                len = snprintf(buf, sizeof(buf), SCRIPT_SECTION_FMT, i);
            
            fname = newStringUTF8(buf, len);
            btData.scriptNames.insert(buf, scriptName);
            
            
            // if the script name starts with |s|, only execute
            // it if "s" is the same first letter as the platform
            // we're running on
            
            // |W| - Windows, |M| - Mac OS X, |L| - Linux
            
            // Adding a 'not' symbol means it WON'T run on that
            // platform (i.e. |!W| won't run on Windows)
            /*
             if (scriptName[0] == '|') {
             int len = strlen(scriptName);
             if (len > 2) {
             if (scriptName[1] == '!' && len > 3 &&
             scriptName[3] == scriptName[0]) {
             if (toupper(scriptName[2]) == platform[0])
             continue;
             }
             if (scriptName[2] == scriptName[0] &&
             toupper(scriptName[1]) != platform[0])
             continue;
             }
             }
             */
            
            int state;
            
            // Log every 50th script and the first 10
            if (i < 10 || i % 50 == 0) {
                fprintf(stderr, "[MKXP-Z] DEBUG: Executing script %ld: %s\n", i, scriptName);
            }
            
            evalString(string, fname, &state);
            if (state) {
                fprintf(stderr, "[MKXP-Z] DEBUG: Script %ld (%s) caused error state: %d\n", i, scriptName, state);
                break;
            }
        }
        
        VALUE exc = rb_gv_get("$!");
        if (rb_obj_class(exc) != getRbData()->exc[Reset])
            break;
        
        if (processReset(false))
            break;
    }
}

// =============================================================================
// ADVANCED ERROR LOGGING SYSTEM
// =============================================================================
// Log levels: 0=SILENT, 1=ERROR, 2=WARN, 3=INFO, 4=DEBUG, 5=TRACE
// Set via environment variable MKXPZ_LOG_LEVEL or default to 3 (INFO)
// =============================================================================

enum MKXPZLogLevel {
    LOG_SILENT = 0,
    LOG_ERROR = 1,
    LOG_WARN = 2,
    LOG_INFO = 3,
    LOG_DEBUG = 4,
    LOG_TRACE = 5
};

static MKXPZLogLevel getLogLevel() {
    static MKXPZLogLevel cachedLevel = LOG_INFO;
    static bool initialized = false;
    
    if (!initialized) {
        const char* envLevel = getenv("MKXPZ_LOG_LEVEL");
        if (envLevel) {
            int level = atoi(envLevel);
            if (level >= LOG_SILENT && level <= LOG_TRACE) {
                cachedLevel = static_cast<MKXPZLogLevel>(level);
            }
        }
        initialized = true;
    }
    return cachedLevel;
}

static void logMessage(MKXPZLogLevel level, const char* format, ...) {
    if (level > getLogLevel()) return;
    
    const char* levelStr = "";
    switch (level) {
        case LOG_ERROR: levelStr = "ERROR"; break;
        case LOG_WARN:  levelStr = "WARN "; break;
        case LOG_INFO:  levelStr = "INFO "; break;
        case LOG_DEBUG: levelStr = "DEBUG"; break;
        case LOG_TRACE: levelStr = "TRACE"; break;
        default: break;
    }
    
    // Timestamp
    time_t now = time(nullptr);
    struct tm* tm_info = localtime(&now);
    char timestamp[20];
    strftime(timestamp, sizeof(timestamp), "%H:%M:%S", tm_info);
    
    fprintf(stderr, "[%s][MKXP-Z][%s] ", timestamp, levelStr);
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fprintf(stderr, "\n");
}

// Enhanced exception analysis - provides context for common Ruby errors
static std::string analyzeException(VALUE exc, VALUE msg, VALUE excName) {
    std::string analysis;
    const char* msgStr = RSTRING_PTR(msg);
    const char* nameStr = RSTRING_PTR(excName);
    
    // NoMethodError analysis
    if (strstr(nameStr, "NoMethodError")) {
        if (strstr(msgStr, "private method")) {
            analysis = "\n  💡 TIP: A private method is being called from outside its class.\n"
                       "     Common causes:\n"
                       "     - 'dispose' called on a custom class that inherits from Sprite/Bitmap but defines dispose as private\n"
                       "     - Method visibility was changed in a parent class\n"
                       "     - Ruby version compatibility issue (method visibility rules changed)\n"
                       "     FIX: Make the method public or use 'send(:method_name)' to bypass visibility";
        } else if (strstr(msgStr, "undefined method")) {
            analysis = "\n  💡 TIP: Method doesn't exist on this object.\n"
                       "     Common causes:\n"
                       "     - Object is nil when method is called\n"
                       "     - Typo in method name\n"
                       "     - Missing require/load statement\n"
                       "     - Class doesn't inherit from expected parent";
        }
    }
    // NameError analysis
    else if (strstr(nameStr, "NameError")) {
        if (strstr(msgStr, "uninitialized constant")) {
            analysis = "\n  💡 TIP: Class or constant not defined.\n"
                       "     Common causes:\n"
                       "     - Script load order issue - class used before it's defined\n"
                       "     - Missing require statement\n"
                       "     - Typo in class/constant name";
        }
    }
    // TypeError analysis
    else if (strstr(nameStr, "TypeError")) {
        analysis = "\n  💡 TIP: Type mismatch in operation.\n"
                   "     Common causes:\n"
                   "     - nil passed where object expected\n"
                   "     - Wrong argument type to method\n"
                   "     - Implicit type conversion failed";
    }
    // LoadError analysis
    else if (strstr(nameStr, "LoadError")) {
        if (strstr(msgStr, ".dll") || strstr(msgStr, "Win32") || strstr(msgStr, "RGSS")) {
            analysis = "\n  💡 TIP: Windows-specific library not available on iOS.\n"
                       "     This is expected - MKXP-Z provides stubs for common DLLs.\n"
                       "     If this script is optional, consider adding error handling.";
        }
    }
    // ArgumentError analysis
    else if (strstr(nameStr, "ArgumentError")) {
        analysis = "\n  💡 TIP: Wrong number or type of arguments.\n"
                   "     Common causes:\n"
                   "     - Method signature changed between Ruby versions\n"
                   "     - Optional arguments handling changed";
    }
    
    return analysis;
}

// Format backtrace for clean logging (filters noise, highlights important frames)
static void logFormattedBacktrace(VALUE bt, const BacktraceData &btData, MKXPZLogLevel level) {
    if (level > getLogLevel()) return;
    
    long btlen = RARRAY_LEN(bt);
    if (btlen == 0) return;
    
    logMessage(level, "━━━ BACKTRACE (%ld frames) ━━━", btlen);
    
    // Show first 10 frames in detail, then summarize
    long showDetailed = (btlen < 10) ? btlen : 10;
    
    for (long i = 0; i < btlen; ++i) {
        VALUE entry = rb_ary_entry(bt, i);
        const char* frameStr = RSTRING_PTR(entry);
        
        // Skip Ruby internal frames for cleaner output
        if (strstr(frameStr, "ruby:") || strstr(frameStr, "<internal:")) {
            if (i < showDetailed) {
                // Still mention it exists
                logMessage(LOG_TRACE, "  %ld: [internal] %s", i, frameStr);
            }
            continue;
        }
        
        if (i < showDetailed) {
            // Highlight game script frames
            if (strstr(frameStr, "Section") || strstr(frameStr, "{")) {
                logMessage(level, "  📍 %ld: %s", i, frameStr);
            } else {
                logMessage(level, "     %ld: %s", i, frameStr);
            }
        } else if (i == showDetailed) {
            logMessage(level, "  ... (%ld more frames, set MKXPZ_LOG_LEVEL=5 to see all)", btlen - showDetailed);
        } else if (getLogLevel() >= LOG_TRACE) {
            logMessage(LOG_TRACE, "     %ld: %s", i, frameStr);
        }
    }
    
    logMessage(level, "━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
}

// Main enhanced exception logging function
static void logException(VALUE exc, const BacktraceData &btData) {
    VALUE bt = rb_funcall2(exc, rb_intern("backtrace"), 0, NULL);
    VALUE msg = rb_funcall2(exc, rb_intern("message"), 0, NULL);
    VALUE excName = rb_class_path(rb_obj_class(exc));
    
    logMessage(LOG_ERROR, "");
    logMessage(LOG_ERROR, "╔══════════════════════════════════════════════════════════════════╗");
    logMessage(LOG_ERROR, "║                    RUBY EXCEPTION OCCURRED                        ║");
    logMessage(LOG_ERROR, "╚══════════════════════════════════════════════════════════════════╝");
    logMessage(LOG_ERROR, "");
    logMessage(LOG_ERROR, "🔴 Exception: %s", RSTRING_PTR(excName));
    logMessage(LOG_ERROR, "📝 Message: %s", RSTRING_PTR(msg));
    
    // Get first backtrace entry for location
    if (RARRAY_LEN(bt) > 0) {
        VALUE bt0 = rb_ary_entry(bt, 0);
        logMessage(LOG_ERROR, "📍 Location: %s", RSTRING_PTR(bt0));
    }
    
    // Provide helpful analysis
    std::string analysis = analyzeException(exc, msg, excName);
    if (!analysis.empty()) {
        logMessage(LOG_WARN, "%s", analysis.c_str());
    }
    
    logMessage(LOG_ERROR, "");
    
    // Log backtrace
    logFormattedBacktrace(bt, btData, LOG_ERROR);
    
    // Log full exception for DEBUG level
    if (getLogLevel() >= LOG_DEBUG) {
        logMessage(LOG_DEBUG, "");
        logMessage(LOG_DEBUG, "Full Ruby exception output:");
        VALUE ds = rb_sprintf("%" PRIsVALUE ": %" PRIsVALUE " (%" PRIsVALUE ")",
#if RAPI_MAJOR >= 2
                              rb_ary_entry(bt, 0), exc, excName);
#else
                              RSTRING_PTR(rb_ary_entry(bt, 0)), RSTRING_PTR(exc), RSTRING_PTR(excName));
#endif
        logMessage(LOG_DEBUG, "%s", StringValueCStr(ds));
    }
}

// Script execution logging helpers
static void logScriptExecution(long scriptIndex, const char* scriptName, MKXPZLogLevel level) {
    logMessage(level, "Executing script %ld: %s", scriptIndex, scriptName);
}

static void logScriptError(long scriptIndex, const char* scriptName, int errorState) {
    logMessage(LOG_ERROR, "Script %ld (%s) failed with error state: %d", scriptIndex, scriptName, errorState);
}

// =============================================================================
// END ADVANCED ERROR LOGGING SYSTEM
// =============================================================================

static void showExc(VALUE exc, const BacktraceData &btData) {
    // First, log the exception with our enhanced system
    logException(exc, btData);
    
    // Then continue with original showExc logic for message box display
    VALUE bt = rb_funcall2(exc, rb_intern("backtrace"), 0, NULL);
    VALUE msg = rb_funcall2(exc, rb_intern("message"), 0, NULL);
    VALUE bt0 = rb_ary_entry(bt, 0);
    VALUE name = rb_class_path(rb_obj_class(exc));
    
    VALUE ds = rb_sprintf("%" PRIsVALUE ": %" PRIsVALUE " (%" PRIsVALUE ")",
#if RAPI_MAJOR >= 2
                          bt0, exc, name);
#else
    // Ruby 1.9's version of this function needs char*
    RSTRING_PTR(bt0), RSTRING_PTR(exc), RSTRING_PTR(name));
#endif
    /* omit "useless" last entry (from ruby:1:in `eval') */
    for (long i = 1, btlen = RARRAY_LEN(bt) - 1; i < btlen; ++i)
        rb_str_catf(ds, "\n\tfrom %" PRIsVALUE,
#if RAPI_MAJOR >= 2
                    rb_ary_entry(bt, i));
#else
    RSTRING_PTR(rb_ary_entry(bt, i)));
#endif
    Debug() << StringValueCStr(ds);
    
    char *s = RSTRING_PTR(bt0);
    
    char line[16];
    std::string file(512, '\0');
    
    char *p = s + strlen(s);
    char *e;
    
    while (p != s)
        if (*--p == ':')
            break;
    
    e = p;
    
    while (p != s)
        if (*--p == ':')
            break;
    
    /* s         p  e
     * SectionXXX:YY: in 'blabla' */
    
    *e = '\0';
    strncpy(line, *p ? p + 1 : p, sizeof(line));
    line[sizeof(line) - 1] = '\0';
    *e = ':';
    e = p;
    
    /* s         e
     * SectionXXX:YY: in 'blabla' */
    
    *e = '\0';
    strncpy(&file[0], s, file.size());
    *e = ':';
    
    /* Shrink to fit */
    file.resize(strlen(file.c_str()));
    file = btData.scriptNames.value(file, file);
    
    std::string ms(640, '\0');
    snprintf(&ms[0], ms.size(), "Script '%s' line %s: %s occurred.\n\n%s",
             file.c_str(), line, RSTRING_PTR(name), RSTRING_PTR(msg));
    
    showMsg(ms);
}

// Note: ruby_init_stack needs a LOCAL stack variable, not static!

// Forward declaration for static Ruby extensions initializer (C function from libruby.a)
extern "C" void Init_ext(void);
extern "C" void Init_zlib(void);  // For direct zlib initialization on iOS

static void mriBindingExecute() {
    // CRITICAL: Stack anchor must be declared at the very top of this function and remain
    // in scope for the entire duration of Ruby execution. Ruby's GC uses this to scan the stack.
    // Making it volatile prevents compiler optimizations that could move or eliminate it.
    volatile VALUE stack_anchor_start = 0;
    volatile VALUE stack_anchor_end = 0;
    
    // Track if Ruby has already been initialized - Ruby can only be initialized ONCE per process
    static bool rubyInitialized = false;
    
    // Defensive null check for shState before accessing config
    fprintf(stderr, "[MKXP-Z] DEBUG: mriBindingExecute started, checking shState...\n");
    if (!shState) {
        fprintf(stderr, "[MKXP-Z] ERROR: SharedState is null! Cannot proceed.\n");
        return;
    }
    fprintf(stderr, "[MKXP-Z] DEBUG: shState is valid (%p)\n", (void*)shState);
    
    Config &conf = shState->rtData().config;
    fprintf(stderr, "[MKXP-Z] DEBUG: Config loaded, RGSS version: %d\n", conf.rgssVersion);
    
    // iOS Font Fix: Increase fontScale if it's at default value
    // Pokemon Essentials and other games using custom fonts may have clipped text
    // because iOS screen scaling differs from Windows. A higher fontScale helps
    // ensure the full glyph height is rendered.
    if (conf.fontScale < 0.1f || conf.fontScale == 1.0f) {
        conf.fontScale = 1.5f;
        fprintf(stderr, "[MKXP-Z] INFO: iOS font fix applied - fontScale set to %.1f\n", conf.fontScale);
    }
    
#if RAPI_MAJOR >= 2
    // Ruby initialization can only happen ONCE per process
    // Calling ruby_init() again will cause "encoding name was somehow registered twice" crash
    if (!rubyInitialized) {
        /* Normally only a ruby executable would do a sysinit,
         * but not doing it will lead to crashes due to closed
         * stdio streams on some platforms (eg. Windows) */
        // iOS fix: Provide valid argc/argv to avoid null pointer crash in ruby_sysinit
        fprintf(stderr, "[MKXP-Z] DEBUG: About to call ruby_sysinit...\n");
        static char progname[] = "mkxp-z";
        static char *ios_argv[] = { progname, nullptr };
        int argc = 1;
        char **argv = ios_argv;
        ruby_sysinit(&argc, &argv);
        fprintf(stderr, "[MKXP-Z] DEBUG: ruby_sysinit completed\n");
        
        // iOS fix: Use explicit ruby_init_stack with LOCAL variable from function top scope
        // CRITICAL: The stack variable MUST remain in scope for the ENTIRE Ruby execution!
        // Ruby uses this to determine the stack base for GC scanning.
        // If the variable goes out of scope, the GC will access invalid memory causing EXC_BAD_ACCESS.
        // We use the volatile stack_anchor_start declared at the top of this function.
        fprintf(stderr, "[MKXP-Z] DEBUG: About to call ruby_init_stack with stack anchor at %p...\n", 
                (void*)&stack_anchor_start);
        ruby_init_stack((void*)&stack_anchor_start);
        fprintf(stderr, "[MKXP-Z] DEBUG: ruby_init_stack completed\n");
        
        fprintf(stderr, "[MKXP-Z] DEBUG: About to call ruby_init...\n");
        ruby_init();
        fprintf(stderr, "[MKXP-Z] DEBUG: ruby_init completed\n");
        
        // Initialize statically linked Ruby extensions (including zlib)
        fprintf(stderr, "[MKXP-Z] DEBUG: About to call Init_ext for static extensions...\n");
        Init_ext();
        fprintf(stderr, "[MKXP-Z] DEBUG: Init_ext completed - static extensions loaded\n");
        
        rubyInitialized = true;
    } else {
        fprintf(stderr, "[MKXP-Z] DEBUG: Ruby already initialized, skipping init calls\n");
        // Even when reusing Ruby, we need to ensure the stack is properly anchored
        // This is important for proper GC behavior across game launches
        ruby_init_stack((void*)&stack_anchor_start);
    }
    
    fprintf(stderr, "[MKXP-Z] DEBUG: About to call ruby_options...\n");
    std::vector<const char*> rubyArgsC{"mkxp-z"};
    rubyArgsC.push_back("-e ");
    void *node;
    if (conf.jit.enabled) {
#if RAPI_FULL >= 310
        // Ruby v3.1.0 renamed the --jit options to --mjit.
        std::string verboseLevel("--mjit-verbose=");
        std::string maxCache("--mjit-max-cache=");
        std::string minCalls("--mjit-min-calls=");
        rubyArgsC.push_back("--mjit");
#else
        std::string verboseLevel("--jit-verbose=");
        std::string maxCache("--jit-max-cache=");
        std::string minCalls("--jit-min-calls=");
        rubyArgsC.push_back("--jit");
#endif
        verboseLevel += std::to_string(conf.jit.verboseLevel);
        maxCache += std::to_string(conf.jit.maxCache);
        minCalls += std::to_string(conf.jit.minCalls);

        rubyArgsC.push_back(verboseLevel.c_str());
        rubyArgsC.push_back(maxCache.c_str());
        rubyArgsC.push_back(minCalls.c_str());
        node = ruby_options(rubyArgsC.size(), const_cast<char**>(rubyArgsC.data()));
    } else if (conf.yjit.enabled) {
        rubyArgsC.push_back("--yjit");
        // TODO: Maybe support --yjit-exec-mem-size, --yjit-call-threshold
        node = ruby_options(rubyArgsC.size(), const_cast<char**>(rubyArgsC.data()));
    } else {
        node = ruby_options(rubyArgsC.size(), const_cast<char**>(rubyArgsC.data()));
    }
    fprintf(stderr, "[MKXP-Z] DEBUG: ruby_options completed, node=%p\n", node);
    
    int state;
    bool valid = ruby_executable_node(node, &state);
    if (valid)
        state = ruby_exec_node(node);
    if (state || !valid) {
        // The message is formatted for and automatically spits
        // out to the terminal, so let's leave it that way for now
        /*
         VALUE exc = rb_errinfo();
         #if RAPI_FULL >= 250
         VALUE msg = rb_funcall(exc, rb_intern("full_message"), 0);
         #else
         VALUE msg = rb_funcall(exc, rb_intern("message"), 0);
         #endif
         */
        showMsg("An error occurred while initializing Ruby. (Invalid JIT settings?)");
        ruby_cleanup(state);
        shState->rtData().rqTermAck.set();
        return;
    }
    rb_enc_set_default_internal(rb_enc_from_encoding(rb_utf8_encoding()));
    rb_enc_set_default_external(rb_enc_from_encoding(rb_utf8_encoding()));
#else
    ruby_init();
    rb_eval_string("$KCODE='U'");
#ifdef __WIN32__
    if (!conf.winConsole) {
        VALUE iostr = rb_str_new2("NUL");
        // Sysinit isn't a thing yet, so send io to /dev/null instead
        rb_funcall(rb_gv_get("$stderr"), rb_intern("reopen"), 1, iostr);
        rb_funcall(rb_gv_get("$stdout"), rb_intern("reopen"), 1, iostr);
    }
#endif
#endif
    
    topSelf = rb_eval_string("self");
    
    VALUE rbArgv = rb_get_argv();
    for (const auto &str : conf.launchArgs)
        rb_ary_push(rbArgv, rb_utf8_str_new_cstr(str.c_str()));
    
    // Duplicates get pushed for some reason
    rb_funcall(rbArgv, rb_intern("uniq!"), 0);
    
    VALUE lpaths = rb_gv_get(":");
    rb_ary_clear(lpaths);
    
#if defined(MKXPZ_BUILD_XCODE) && RAPI_MAJOR >= 2
    std::string resPath = mkxp_fs::getResourcePath();
    resPath += "/Ruby/" + std::to_string(RAPI_MAJOR) + "." + std::to_string(RAPI_MINOR) + ".0";
    rb_ary_push(lpaths, rb_str_new(resPath.c_str(), resPath.size()));
#endif
    
    if (!conf.rubyLoadpaths.empty()) {
        /* Setup custom load paths */
        for (size_t i = 0; i < conf.rubyLoadpaths.size(); ++i) {
            std::string &path = conf.rubyLoadpaths[i];
            
            VALUE pathv = rb_str_new(path.c_str(), path.size());
            rb_ary_push(lpaths, pathv);
        }
    }
#ifndef WORKDIR_CURRENT
    else {
        rb_ary_push(lpaths, rb_utf8_str_new_cstr(mkxp_fs::getCurrentDirectory().c_str()));
    }
#endif
    
    RbData rbData;
    shState->setBindingData(&rbData);
    BacktraceData btData;
    
    mriBindingInit();
    
    std::string &customScript = conf.customScript;
    if (!customScript.empty())
        runCustomScript(customScript);
    else
        runRMXPScripts(btData);
    
#if RAPI_FULL > 187
    VALUE exc = rb_errinfo();
#else
    VALUE exc = rb_gv_get("$!");
#endif
    if (!NIL_P(exc) && !rb_obj_is_kind_of(exc, rb_eSystemExit))
        showExc(exc, btData);
    
    ruby_cleanup(0);
    
    // Mark end of Ruby execution scope - this helps ensure stack_anchor_start stays in scope
    stack_anchor_end = 1;
    (void)stack_anchor_end;  // Suppress unused variable warning
    
    shState->rtData().rqTermAck.set();
}

static void mriBindingTerminate() { throw Exception(Exception::SystemExit, " "); }

static void mriBindingReset() { throw Exception(Exception::Reset, " "); }
