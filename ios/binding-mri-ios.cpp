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
    MKXP_DEBUG_LOG("DEBUG: mriBindingInit starting...");
    MKXP_DEBUG_LOG("DEBUG: tableBindingInit...");
    tableBindingInit();
    MKXP_DEBUG_LOG("DEBUG: etcBindingInit...");
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
    
    MKXP_DEBUG_LOG("DEBUG: inputBindingInit...");
    inputBindingInit();
    MKXP_DEBUG_LOG("DEBUG: audioBindingInit...");
    audioBindingInit();
    MKXP_DEBUG_LOG("DEBUG: graphicsBindingInit...");
    graphicsBindingInit();
    
    MKXP_DEBUG_LOG("DEBUG: fileIntBindingInit...");
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
    Debug() << "Zlib initialized directly for static linking (StringIO will be polyfilled)";
    
    // =============================================================================
    // SUPERCLASS MISMATCH FIX FOR OLD RPG MAKER SCRIPTS
    // In Ruby 3.0+, redefining a class with a different superclass raises TypeError.
    // Old RPG Maker scripts sometimes do this (e.g., Window_Message < Window_Selectable
    // when it was already defined as Window_Message < Window_Base).
    // This fix patches Class.new to handle this gracefully by returning the existing class.
    // =============================================================================
    int state;
    rb_eval_string_protect(
        "class << Class\n"
        "  alias :__mkxpz_original_new__ :new\n"
        "  def new(superclass = Object, &block)\n"
        "    __mkxpz_original_new__(superclass, &block)\n"
        "  rescue TypeError => e\n"
        "    if e.message.include?('superclass mismatch')\n"
        "      # Extract class name from the error message or context\n"
        "      # Just return nil - the class already exists and will be used\n"
        "      nil\n"
        "    else\n"
        "      raise\n"
        "    end\n"
        "  end\n"
        "end\n"
        "\n"
        "# Also patch the 'class' keyword behavior by catching the error at eval level\n"
        "# This is done by setting a trace that rescues and continues\n"
        "module MKXPZSuperclassFix\n"
        "  @eval_stack = {}\n"
        "  def self.wrap_eval(caller_binding, code, *args)\n"
        "    binding_obj = args[0] || caller_binding\n"
        "    filename = args[1] || '(eval)'\n"
        "    lineno = args[2] || 1\n"
        "    \n"
        "    @eval_stack[filename] ||= 0\n"
        "    if @eval_stack[filename] > 5\n"
        "       $stderr.puts \"[MKXP-Z] FATAL: Too many recursive fixes for #{filename}. Breaking loop to prevent SystemStackError.\"\n"
        "       return eval(code, binding_obj, filename, lineno) rescue nil\n"
        "    end\n"
        "    \n"
        "    begin\n"
        "      @eval_stack[filename] += 1\n"
        "      eval(code, binding_obj, filename, lineno)\n"
        "    rescue SyntaxError => e\n"
        "      if e.message.include?('unexpected keyword arg given in index assignment')\n"
        "        $stderr.puts \"[MKXP-Z] SyntaxError (Keyword Arg in Index Assignment) detected in #{filename}\"\n"
        "        pattern = /\\[([^\\[\\]:]+:\\s*[^\\[\\]:]+)\\]\\s*=/\n"
        "        if code =~ pattern\n"
        "          new_code = code.gsub(pattern, '[{\\1}] =')\n"
        "          $stderr.puts \"[MKXP-Z] Fixed: Wrapped keyword args in braces for index assignment\"\n"
        "          return wrap_eval(caller_binding, new_code, binding_obj, filename, lineno)\n"
        "        else\n"
        "          $stderr.puts \"[MKXP-Z] FAILED to find faulty index assignment in code\"\n"
        "        end\n"
        "        raise\n"
        "      else\n"
        "        raise\n"
        "      end\n"
        "    rescue TypeError => e\n"
        "      if e.message.include?('superclass mismatch')\n"
        "        # 1. Extract class name\n"
        "        if e.message =~ /for class\\s+([\\w:]+)/\n"
        "          target_class = $1\n"
        "          $stderr.puts \"[MKXP-Z] Superclass mismatch detected for: #{target_class} in #{filename}\"\n"
        "\n"
        "          # 2. Relaxed Regex: Support Optional namespace prefix\n"
        "          pattern = /class\\s+((?:[\\w:]*::)?)#{Regexp.escape(target_class)}\\s*<\\s*[^;\\n]+/\n"
        "\n"
        "          if code =~ pattern\n"
        "            # 3. Apply Fix: Remove superclass inheritance\n"
        "            # IMPORTANT: We use sub! to ensure we only change it once per recursion\n"
        "            new_code = code.sub(pattern, \"class \\\\1#{target_class}\")\n"
        "            $stderr.puts \"[MKXP-Z] Fixed: Removed superclass from #{target_class} (Prefix: \\\\1)\"\n"
        "            \n"
        "            # Recursive call\n"
        "            return wrap_eval(caller_binding, new_code, binding_obj, filename, lineno)\n"
        "          else\n"
        "            $stderr.puts \"[MKXP-Z] FAILED to find class definition in code for #{target_class}\"\n"
        "          end\n"
        "        end\n"
        "        raise # Re-raise if we couldn't handle it\n"
        "      else\n"
        "        raise\n"
        "      end\n"
        "    rescue Exception => e\n"
        "      raise # Passing through other exceptions (like SystemStackError if it already started)\n"
        "    ensure\n"
        "      @eval_stack[filename] -= 1 if @eval_stack[filename] > 0\n"
        "    end\n"
        "  end\n"
        "end\n",
        &state);
    
    if (state == 0) {
        Debug() << "Superclass mismatch fix installed";
    } else {
        Debug() << "Warning: Could not install superclass mismatch fix";
        rb_errinfo();
    }
    
    // For static linking, we must tell Ruby that the feature is already loaded
    // otherwise require 'zlib' will fail because it can't find zlib.so/zlib.rb
    // IMPORTANT: Add entries unconditionally (with duplicate check) since Init_zlib() was called
    rb_eval_string_protect(
        // Add zlib to LOADED_FEATURES so require 'zlib' returns false instead of throwing error
        // Use include? check to avoid duplicates if this runs multiple times
        "$LOADED_FEATURES << 'zlib.so' unless $LOADED_FEATURES.include?('zlib.so'); "
        "$LOADED_FEATURES << 'zlib.rb' unless $LOADED_FEATURES.include?('zlib.rb'); "
        "$LOADED_FEATURES << 'zlib' unless $LOADED_FEATURES.include?('zlib'); "
        "$LOADED_FEATURES << 'stringio.so' unless $LOADED_FEATURES.include?('stringio.so'); "
        "$LOADED_FEATURES << 'stringio.rb' unless $LOADED_FEATURES.include?('stringio.rb'); "
        "$LOADED_FEATURES << 'stringio' unless $LOADED_FEATURES.include?('stringio'); "
        "class StringIO; "
        "  def initialize(s=''); @s=s.to_s.dup.force_encoding('BINARY'); @p=0; end; "
        "  def self.open(*a); io=new(*a); return io unless block_given?; begin; yield io; ensure; io.close; end; end; "
        "  def string; @s; end; def string=(s); @s=s.to_s.dup.force_encoding('BINARY'); @p=0; end; "
        "  def read(l=nil); return nil if eof? && l; r=l ? @s[@p,l] : @s[@p..-1]; @p+=(l || (@s.length-@p)); r; end; "
        "  def write(s); s=s.to_s; @s[@p,s.length]=s; @p+=s.length; s.length; end; "
        "  def puts(*a); a.each{|x|write(x.to_s+\"\\n\")}; nil; end; "
        "  def gets(sep=$/); return nil if eof?; i=@s.index(sep,@p); if i; r=@s[@p..i]; @p=i+sep.length; else; r=@s[@p..-1]; @p=@s.length; end; r; end; "
        "  def pos; @p; end; def pos=(p); @p=p; end; def rewind; @p=0; end; def eof?; @p>=@s.length; end; "
        "  def seek(p,w=0); case w; when 0; @p=p; when 1; @p+=p; when 2; @p=@s.length+p; end; @p=0 if @p<0; 0; end; "
        "  alias_method :tell, :pos; def close; @c=true; end; def closed?; @c; end; "
        "end unless defined?(StringIO); "
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
    
    // =============================================================================
    // FONT COMPATIBILITY FIX FOR iOS
    // Many Japanese RPG Maker games check for specific fonts that don't exist on iOS.
    // We patch Font.exist? to always return true, allowing the game to use fallback fonts.
    // =============================================================================
    rb_eval_string_protect(
        "class << Font\n"
        "  alias :__mkxpz_original_exist? :exist? rescue nil\n"
        "  def exist?(name)\n"
        "    result = __mkxpz_original_exist?(name) rescue false\n"
        "    unless result\n"
        "      $stderr.puts \"[MKXP-Z][FONT] Font '#{name}' not found, returning true anyway for compatibility\"\n"
        "      $stderr.flush\n"
        "      return true  # Pretend font exists to avoid exit\n"
        "    end\n"
        "    result\n"
        "  end\n"
        "end\n"
        "\n"
        "$stderr.puts '[MKXP-Z][FONT] Font.exist? compatibility patch installed'\n"
        "$stderr.flush\n"
        , &state);
    
    if (state == 0) {
        Debug() << "Font.exist? compatibility patch installed";
    } else {
        Debug() << "Warning: Could not install Font.exist? patch";
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
        "    :pbShowCommands, :pbShowCommandsWithHelp, :pbMessage, :pbMessageDisplay, :getPlaceInGame,\n"
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
        "    [:pbShowCommands, :pbShowCommandsWithHelp, :pbMessage, :pbMessageDisplay, :getPlaceInGame,\n"
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
        MKXP_DEBUG_LOG("DEBUG: mriBindingInit completed successfully");
    } else {
        Debug() << "Warning: Could not install BASS stub";
        MKXP_DEBUG_LOG("DEBUG: mriBindingInit completed with warnings");
        rb_errinfo();
    }
    
    // =============================================================================
    // FIX: Pokemon Essentials and other games use throw :__mkxpz_break__ internally
    // In Ruby 4.0, uncaught throws raise UncaughtThrowError instead of being ignored
    // This patch wraps Kernel#throw to silently handle specific internal symbols
    // =============================================================================
    rb_eval_string_protect(
        "module Kernel\n"
        "  alias :__mkxpz_original_throw :throw\n"
        "  def throw(tag, value = nil)\n"
        "    begin\n"
        "      __mkxpz_original_throw(tag, value)\n"
        "    rescue UncaughtThrowError => e\n"
        "      # List of internal symbols used by games that should be silently ignored\n"
        "      internal_symbols = [:__mkxpz_break__, :break, :__break__, :__mkxpz_loop_break__]\n"
        "      raise unless internal_symbols.include?(tag)\n"
        "      # Return nil for silently caught throws\n"
        "      nil\n"
        "    end\n"
        "  end\n"
        "  module_function :throw\n"
        "end\n",
        &state);
    
    if (state == 0) {
        Debug() << "Kernel#throw uncaught throw handler installed successfully";
        MKXP_DEBUG_LOG("DEBUG: Kernel#throw patch installed");
    } else {
        Debug() << "Warning: Could not install Kernel#throw patch";
        MKXP_DEBUG_LOG("DEBUG: Kernel#throw patch failed");
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
        MKXP_INFO_LOG("Applying legacy Ruby 1.8/1.9 compatibility shims for RGSS%d\n", rgssVer);
        
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
        
        MKXP_INFO_LOG("Legacy Ruby compatibility shims installed for RGSS%d\n", rgssVer);
    } else {
        MKXP_DEBUG_LOG("Skipping legacy shims for RGSS%d (not needed for VX Ace)\n", rgssVer);
    }
    
    // =============================================================================
    // Ruby 3.x/4.0 ALIAS_METHOD COMPATIBILITY FIX
    // In Ruby 1.8/1.9, you could alias a method that doesn't exist yet.
    // In Ruby 3.0+, this raises NameError: undefined method.
    // This shim wraps alias_method to silently ignore undefined methods,
    // allowing the alias to be created later when the original method is defined.
    // Applied to ALL RGSS versions since this is a Ruby version issue, not RGSS.
    // =============================================================================
    rb_eval_string_protect(
        "class Module\n"
        "  # Store original alias_method\n"
        "  alias :__mkxpz_original_alias_method__ :alias_method\n"
        "  \n"
        "  # Override alias_method to handle undefined methods gracefully\n"
        "  def alias_method(new_name, old_name)\n"
        "    # Check if the original method exists\n"
        "    if method_defined?(old_name) || private_method_defined?(old_name) || protected_method_defined?(old_name)\n"
        "      __mkxpz_original_alias_method__(new_name, old_name)\n"
        "    else\n"
        "      # Method doesn't exist yet - store for later application\n"
        "      # This allows scripts to define the method after the alias line\n"
        "      @__mkxpz_pending_aliases__ ||= []\n"
        "      @__mkxpz_pending_aliases__ << [new_name, old_name]\n"
        "      # Also hook method_added to apply pending aliases\n"
        "      unless @__mkxpz_method_added_hooked__\n"
        "        @__mkxpz_method_added_hooked__ = true\n"
        "        class << self\n"
        "          alias :__mkxpz_original_method_added__ :method_added rescue nil\n"
        "          def method_added(name)\n"
        "            __mkxpz_original_method_added__(name) if respond_to?(:__mkxpz_original_method_added__, true) rescue nil\n"
        "            # Check pending aliases\n"
        "            if @__mkxpz_pending_aliases__\n"
        "              @__mkxpz_pending_aliases__.each do |new_name, old_name|\n"
        "                if name.to_sym == old_name.to_sym\n"
        "                  __mkxpz_original_alias_method__(new_name, old_name) rescue nil\n"
        "                end\n"
        "              end\n"
        "              # Remove applied aliases\n"
        "              @__mkxpz_pending_aliases__.reject! { |n, o| o.to_sym == name.to_sym }\n"
        "            end\n"
        "          end\n"
        "        end\n"
        "      end\n"
        "    end\n"
        "  end\n"
        "end\n",
        &state);
    
    if (state == 0) {
        Debug() << "Ruby 3.x alias_method compatibility shim installed";
    } else {
        Debug() << "Warning: Could not install alias_method compatibility shim";
        rb_errinfo();
    }
    
    // =============================================================================
    // NIL COMPARISON FIX (Added 2026-02-11)
    // Many older RPG Maker scripts (like ISPDS) have bugs where they compare nil with numbers.
    // E.g. "if variable < 0.0" where variable is nil crashes with ArgumentError.
    // This patch makes nil comparable with numbers, treating nil as 0, preventing crashes.
    // =============================================================================
    rb_eval_string_protect(
        "class NilClass\n"
        "  include Comparable\n"
        "  def <=>(other)\n"
        "    return 0 <=> other if other.is_a?(Numeric)\n"
        "    nil\n"
        "  end\n"
        "end\n",
        &state);
    
    if (state == 0) {
        Debug() << "NilClass comparison fix installed successfully";
    } else {
        Debug() << "Warning: Could not install NilClass comparison fix";
        rb_errinfo();
    }

    // Set $stdout and its ilk accordingly on Windows
    // I regret teaching you that word
#ifdef __WIN32__
    if (shState->config().winConsole)
        configureWindowsStreams();
#endif
}

static void showMsg(const std::string &msg) {
#if defined(IOS_PLATFORM) || defined(__IPHONEOS__)
    // iOS: Don't use EventThread.showMessageBox() - it causes deadlock!
    // iOS doesn't run EventThread.process() loop, so pushing SDL events
    // to show message boxes will hang forever waiting for processing.
    // Instead, log the error and store it in rgssErrorMsg for Swift to display.
    fprintf(stderr, "[MKXP-Z] showMsg: %s\n", msg.c_str());
    
    // Store error in threadData for Swift crash handler to pick up
    if (shState) {
        shState->rtData().rgssErrorMsg = msg;
        // Request termination so Swift knows the game has stopped
        shState->rtData().rqTermAck.set();
    }
#else
    shState->eThread().showMessageBox(msg.c_str());
#endif
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
 * - "alias new old" to safe form that handles undefined methods
 * 
 * Note: This is a best-effort conversion. Complex edge cases might not be handled.
 */
static std::string preprocessRuby18Syntax(const std::string& script) {
    if (script.empty()) return script;
    
    std::string result = script;
    
    // Check for UTF-8 BOM (\xEF\xBB\xBF) and remove it if present
    // This fixes syntax errors like "unexpected '=', ignoring it" when a script starts with BOM + "=begin"
    if (result.size() >= 3) {
        const unsigned char* bytes = reinterpret_cast<const unsigned char*>(result.c_str());
        if (bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF) {
            result.erase(0, 3);
        }
    }
    
    try {
        // =========================================================================
        // 1. Convert "when X:" syntax to "when X then" (Ruby 1.8 -> 3.x)
        // =========================================================================
        // Ruby 1.8 allowed "when X:" syntax, but Ruby 3.x requires "when X then" or "when X;"
        // 
        // Case A: "when X:" at end of line (possibly with comment)
        //         e.g., "when 1:" or "when 'foo': # comment"
        // Case B: "when X:" followed by code on same line  
        //         e.g., "when 3: next getID(PBTypes,:DRAGON)"
        //
        // We need to be careful not to match symbolic literals like ":DRAGON"
        // The key insight: after "when", the first ":" that is NOT preceded by 
        // a comma/space (symbol context) is the case delimiter.
        //
        // Strategy: Match "when" + simple literals (numbers, strings, symbols) + ":"
        std::string whenPred = R"(-?\d+(?:\.\d+)?|[@$]{0,2}[a-zA-Z_]\w*[?!]?(?:::[a-zA-Z_]\w*[?!]?)*|:[a-zA-Z_]\w*[?!]?|'[^']*'|"[^"]*")";
        
        // Simple case: when followed by a single predicate, then ":"
        std::regex whenColonPatternSimple(
            R"((when\s+)()" + whenPred + R"()\s*:(?!:)\s*)",
            std::regex::multiline
        );
        result = std::regex_replace(result, whenColonPatternSimple, "$1$2 then ");
        
        // Handle multiple values: when A, B, C:
        std::regex whenColonPatternMulti(
            R"((when\s+)((?:)" + whenPred + R"()(?:\s*,\s*(?:)" + whenPred + R"())*)\s*:(?!:)\s*)",
            std::regex::multiline  
        );
        result = std::regex_replace(result, whenColonPatternMulti, "$1$2 then ");
        
        // =========================================================================
        // 2. Convert unsafe "alias" to safe "alias_method" with rescue
        // =========================================================================
        // In Ruby 3.0+, "alias new old" raises NameError if 'old' doesn't exist.
        // We convert: alias new_name old_name
        // To: alias_method :new_name, :old_name rescue nil
        // 
        // Pattern: alias <new> <old> (not inside string, at start of line or after semicolon)
        // Captures: (alias)\s+(:?\w+)\s+(:?\w+)
        // Note: We need to be careful not to match "alias_method" itself
        std::regex aliasPattern(
            R"((^|\s|;)(alias)\s+(:?)(\w+)\s+(:?)(\w+)(\s*(?:#.*)?$))",
            std::regex::multiline
        );
        
        // Replace with: alias_method :new, :old unless method_defined?(:new) rescue nil
        // $1 = prefix (whitespace/semicolon), $2 = "alias", $3 = optional colon, $4 = new_name
        // $5 = optional colon, $6 = old_name, $7 = trailing
        // 
        // IMPORTANT: We ADD "unless method_defined?" check to PREVENT recursion!
        // 
        // Problem scenario without check:
        // 1. Plugin loads: alias oldInit initialize  -> oldInit = original
        //    def initialize calls oldInit (OK)
        // 2. Plugin loads AGAIN (duplicate loading): alias oldInit initialize -> oldInit = PATCHED!
        //    def initialize calls oldInit which calls initialize = INFINITE LOOP!
        // 
        // With "unless method_defined?" check:
        // 1. First load: oldInit undefined, alias created -> oldInit = original
        // 2. Second load: oldInit ALREADY defined, alias SKIPPED!
        //    oldInit still points to original = NO RECURSION!
        // 
        // This fixes "SystemStackError: stack level too deep" in games like Pokemon Anil
        // where plugins like "[Advanced Items - Field Moves]" create alias chains.
        // NOTE: We also check private_method_defined? because methods like "initialize"
        // are private in Ruby. method_defined? only checks public/protected methods,
        // so without this, private aliases (e.g. alias old_init initialize) would be
        // re-created on duplicate loads, causing infinite recursion.
        result = std::regex_replace(result, aliasPattern, 
            "$1alias_method :$4, :$6 unless (method_defined?(:$4) || private_method_defined?(:$4)) rescue nil$7");
        
        // =========================================================================
        // 3. Handle superclass mismatch for class definitions (DISABLED)
        // =========================================================================
        // NOTE: This transformation was too aggressive and caused side effects.
        // The superclass mismatch issue is now handled differently:
        // - A Ruby shim is installed in mriBindingInit that catches TypeError
        //   during class definition and reopens the class without inheritance.
        // - This is safer because it only triggers when an actual error occurs.
        // =========================================================================
        // =========================================================================
        // 4. Convert "BEGIN { ... }" blocks to immediate execution
        // =========================================================================
        // In Ruby, BEGIN blocks are only permitted at file toplevel.
        // When scripts are executed via eval(), BEGIN causes:
        //   "BEGIN is permitted only at toplevel"
        //
        // Solution: Convert "BEGIN {" to "(proc {" so the block becomes a proc
        // that we can call immediately. The closing "}" of BEGIN block stays as is.
        // We add ".call" after the block by finding END blocks or doing post-processing.
        //
        // Simpler approach: Just replace "BEGIN {" with "(proc {" 
        // The user's code will need the matching "})" but that might break.
        //
        // Even simpler and safer: Replace "BEGIN {" with empty comment and "{"
        // Actually, let's just wrap in a method call to ensure execution:
        // "BEGIN {" -> "(lambda {"
        // And we'll need to handle the closing brace separately.
        //
        // SAFEST: Match the entire BEGIN block if we can find balanced braces.
        // For now, let's try: "BEGIN {" -> "__mkxpz_begin_block = proc {"
        // Then at the end of the block, it becomes: "}; __mkxpz_begin_block.call"
        // But this requires finding the closing brace which is complex.
        //
        // ALTERNATIVE: Just remove "BEGIN" keyword entirely
        // The remaining "{ ... }" will be parsed as a block/hash depending on context.
        // In toplevel, it's a hash literal which will be evaluated and discarded.
        // But the code inside will NOT execute!
        //
        // CORRECT APPROACH: Replace "BEGIN {" with "(->{"
        // This creates a lambda, and we need to find the matching "}" and add ").call"
        // 
        // For simplicity, let's do a two-pass approach:
        // 1. Replace "BEGIN {" with a unique marker "(->{ # __MKXPZ_BEGIN__"
        // 2. After all processing, scan for the marker and its matching }, add .call
        
        std::regex beginPattern(
            R"((^|\n|\r)([ \t]*)BEGIN\s*\{)",
            std::regex::multiline
        );
        // Replace with: $1$2(proc { ... content ... }).call
        // We mark the start so we can find the matching end
        // Actually, let's just do: Replace "BEGIN {" with "(proc {"
        // Then scan for unmatched "}" after and replace with "}).call"
        // This is imperfect but works for simple single-block BEGIN statements
        
        if (std::regex_search(result, beginPattern)) {
            // Simple strategy: Replace "BEGIN {" with "(proc {"
            // Then we need to add ".call" after the matching "}"
            // For now, assume BEGIN blocks are at script start and have simple structure
            
            // First, count BEGIN blocks
            std::string temp = result;
            std::smatch match;
            int beginCount = 0;
            while (std::regex_search(temp, match, beginPattern)) {
                beginCount++;
                temp = match.suffix().str();
            }
            
            // Replace BEGIN with proc
            result = std::regex_replace(result, beginPattern, "$1$2(proc {");
            
            // Now we need to find the matching "}" for each BEGIN and add ").call"
            // This is the tricky part - we need to track brace depth
            // For a robust solution, we'd need a proper parser. For now, use heuristic:
            // Look for "}" at the same indentation level as "BEGIN" was
            
            // Simple heuristic for single BEGIN block at start of script:
            // Find the first "}" that appears at start of line (or with same indent)
            // and is not inside a string or comment
            
            // For now, let's try: look for "\n}" or "\r\n}" pattern following a proc block
            // This is error-prone but may work for common cases
            
            // Actually, let's be smarter: Track brace depth starting from each "(proc {"
            std::string newResult;
            newResult.reserve(result.length() + beginCount * 10);
            
            size_t pos = 0;
            int procDepth = 0;
            bool inString = false;
            char stringChar = 0;
            bool inComment = false;
            
            while (pos < result.length()) {
                // Track strings
                if (!inComment && !inString && (result[pos] == '"' || result[pos] == '\'')) {
                    inString = true;
                    stringChar = result[pos];
                    newResult += result[pos++];
                    continue;
                }
                if (inString) {
                    if (result[pos] == '\\' && pos + 1 < result.length()) {
                        newResult += result[pos++];
                        newResult += result[pos++];
                        continue;
                    }
                    if (result[pos] == stringChar) {
                        inString = false;
                    }
                    newResult += result[pos++];
                    continue;
                }
                
                // Track comments
                if (!inComment && result[pos] == '#') {
                    inComment = true;
                    newResult += result[pos++];
                    continue;
                }
                if (inComment && (result[pos] == '\n' || result[pos] == '\r')) {
                    inComment = false;
                    newResult += result[pos++];
                    continue;
                }
                if (inComment) {
                    newResult += result[pos++];
                    continue;
                }
                
                // Track "(proc {" - we look for "proc {" after "("
                if (pos + 7 < result.length() && 
                    result.substr(pos, 7) == "(proc {") {
                    procDepth++;
                    newResult += "(proc {";
                    pos += 7;
                    continue;
                }
                
                // Track braces when we're inside a proc block
                if (procDepth > 0) {
                    if (result[pos] == '{') {
                        procDepth++;
                    } else if (result[pos] == '}') {
                        procDepth--;
                        if (procDepth == 0) {
                            // This is the closing brace of our proc
                            newResult += "}).call";
                            pos++;
                            continue;
                        }
                    }
                }
                
                newResult += result[pos++];
            }
            
            result = newResult;
        }
        
    } catch (const std::regex_error& e) {
        // If regex fails, just return original script
        Debug() << "Ruby 1.8 syntax preprocessor regex error:" << e.what();
        return script;
    }
    
    // =========================================================================
    // 4. Sanitize invalid multibyte escapes in regexes (Fix for Pokemon Iberia)
    // =========================================================================
    // Ruby 3.x is strict about invalid multibyte escape sequences in UTF-8.
    // Some legacy scripts (like PSystem_Utilities) use ranges like \x7f-\x9f 
    // which are invalid in UTF-8 regexes. We strip the high-byte range.
    try {
        std::regex multibyteEscapePattern(R"(\\x7f-\\x9f)");
        result = std::regex_replace(result, multibyteEscapePattern, "\\x7f");
    } catch (const std::regex_error& e) {
         Debug() << "Multibyte escape patch regex error:" << e.what();
    }
    
    return result;
}

// Ruby 3.x compatibility: Check if script uses @@class_variables in toplevel class << self blocks
// In Ruby 3.0+, accessing @@class_variables from toplevel singleton class is forbidden.
// This function detects such patterns to allow wrapping the script.
static bool needsClassVariableCompat(const std::string &script) {
    // Quick check: if no @@, no need to process
    if (script.find("@@") == std::string::npos)
        return false;
    
    // Check for class << self pattern at toplevel
    // This is a heuristic - we look for "class << self" not inside a class definition
    size_t pos = 0;
    int classNestLevel = 0;
    
    while (pos < script.length()) {
        // Skip comments
        if (script[pos] == '#') {
            while (pos < script.length() && script[pos] != '\n')
                pos++;
            continue;
        }
        
        // Skip string literals (simple check)
        if (script[pos] == '"' || script[pos] == '\'') {
            char quote = script[pos++];
            while (pos < script.length() && script[pos] != quote) {
                if (script[pos] == '\\') pos++;
                pos++;
            }
            pos++;
            continue;
        }
        
        // Check for class keyword
        if (pos + 5 < script.length() && 
            script.substr(pos, 5) == "class" &&
            (pos == 0 || !isalnum(script[pos-1])) &&
            (pos + 5 >= script.length() || !isalnum(script[pos+5]))) {
            
            // Check if it's "class << self" at toplevel
            size_t afterClass = pos + 5;
            while (afterClass < script.length() && isspace(script[afterClass]))
                afterClass++;
            
            if (afterClass + 1 < script.length() && 
                script[afterClass] == '<' && script[afterClass+1] == '<') {
                // This is "class << ..."
                if (classNestLevel == 0) {
                    // Toplevel singleton class - needs compat
                    return true;
                }
            } else {
                // Regular class definition
                classNestLevel++;
            }
            pos = afterClass;
            continue;
        }
        
        // Check for module keyword (also increases nesting)
        if (pos + 6 < script.length() && 
            script.substr(pos, 6) == "module" &&
            (pos == 0 || !isalnum(script[pos-1])) &&
            (pos + 6 >= script.length() || !isalnum(script[pos+6]))) {
            classNestLevel++;
            pos += 6;
            continue;
        }
        
        // Check for end keyword
        if (pos + 3 <= script.length() && 
            script.substr(pos, 3) == "end" &&
            (pos == 0 || !isalnum(script[pos-1])) &&
            (pos + 3 >= script.length() || !isalnum(script[pos+3]))) {
            if (classNestLevel > 0) classNestLevel--;
            pos += 3;
            continue;
        }
        
        pos++;
    }
    
    return false;
}

// Wrap script in Object.class_eval to provide class variable context for Ruby 3.x
static std::string wrapScriptForClassVariableCompat(const std::string &script, const char* scriptName) {
    std::string wrapped;
    wrapped.reserve(script.length() + 100);
    wrapped = "Object.class_eval do\n";
    wrapped += script;
    wrapped += "\nend\n";
    
    Debug() << "[MKXP-Z] Ruby 3.x compat: Wrapped script '" << scriptName << "' for class variable access";
    
    return wrapped;
}

// =========================================================================
// Ruby 4.0 Prism Parser Compatibility: Fix "Invalid break" syntax errors
// =========================================================================
// Some legacy RPG Maker scripts have "break" statements inside conditional
// blocks within loops. Ruby 4.0's Prism parser is stricter and may not
// recognize the loop context when parsing individual scripts.
// 
// This fix wraps scripts containing standalone "break" in a loop context
// by replacing "break" with "throw(:__mkxpz_break__)" and wrapping the
// entire loop blocks with catch(:__mkxpz_break__).
//
// 'break' is valid in:
// - loop, while, until, for
// - Blocks passed to iterators (each, map, times, etc.)
// - **Procs created with lambda**
//
// This fix specifically targets scripts known to have this issue.
// =========================================================================

// List of scripts known to have "Invalid break" issues
static const std::vector<std::string> scriptsNeedingBreakFix = {
    "PokemonNotebook",
    "Connect/Register/Login/Deuks",
    // Add more script names here as discovered
};

static bool needsBreakSyntaxFix(const std::string &script, const std::string &scriptName) {
    // Quick check: if no "break", no need to process
    if (script.find("break") == std::string::npos)
        return false;
    
    // Check if this script is in the known list
    for (const auto& knownScript : scriptsNeedingBreakFix) {
        if (scriptName.find(knownScript) != std::string::npos) {
            return true;
        }
    }
    
    return false;
}

// Wrap "break" statements in a way that makes them valid in Ruby 4.0
// Strategy: Replace standalone "break" with "return nil" since the script
// functions are typically called in a loop context and early return is safe.
static std::string fixBreakSyntax(const std::string &script, const char* scriptName) {
    std::string result = script;
    
    // We need to be careful to only replace "break" that is:
    // 1. A standalone keyword (not part of another word)
    // 2. Likely inside an if/elsif/else block within a loop
    
    // For these specific scripts, we'll wrap the entire "loop do" blocks
    // with catch/throw to make break valid.
    
    // Strategy: Find all "loop do" and wrap them with catch(:__mkxpz_break__)
    // Then replace "break" with "throw(:__mkxpz_break__)"
    
    // First, let's try a simpler approach: Replace "break" with "return nil"
    // This works because the break is usually meant to exit a method or block
    
    // More precise: Wrap the script's main loop in a lambda
    // Find patterns like "loop do" and wrap with "catch(:__brk__) { loop do ... end }"
    
    // For now, let's use regex to replace standalone "break" with "(throw :__mkxpz_loop_break__)"
    // and wrap any "loop do" with "catch(:__mkxpz_loop_break__) do loop do"
    
    try {
        // Step 1: Find matching "end" for loop do (simplified - just add closing brace after)
        // This is tricky... let's try a different approach

        // This is tricky... let's try a different approach
        
        // Alternative: Just replace standalone "break" with "throw(:__mkxpz_loop_break__)"
        // and wrap the entire script in a catch block
        std::regex breakPattern(R"(\bbreak\b(?!\s*\())");
        
        // Check if we actually need to modify anything
        if (std::regex_search(result, breakPattern)) {
            // Replace break with throw
            result = std::regex_replace(result, breakPattern, "throw(:__mkxpz_loop_break__)");
            
            Debug() << "[MKXP-Z] SYNTAX FIX: Wrapped 'break' statements in script '" << scriptName << "' with throw(:__mkxpz_loop_break__)";
        }
    } catch (const std::regex_error& e) {
        Debug() << "[MKXP-Z] Break syntax fix regex error: " << e.what();
        return script;
    }
    
    return result;
}

// If the script has throw(:__mkxpz_loop_break__), we need to wrap loop blocks
static std::string wrapLoopsWithCatch(const std::string &script, const char* scriptName) {
    if (script.find("throw(:__mkxpz_loop_break__)") == std::string::npos) {
        return script;
    }
    
    std::string result = script;
    size_t pos = 0;
    int modifications = 0;
    
    // Find all "loop do" or "loop {" patterns and wrap them
    while (pos < result.length()) {
        // Look for "loop" keyword
        size_t loopPos = result.find("loop", pos);
        if (loopPos == std::string::npos) break;
        
        // Verify it's a standalone keyword
        if (loopPos > 0 && (isalnum(result[loopPos - 1]) || result[loopPos - 1] == '_')) {
            pos = loopPos + 4;
            continue;
        }
        
        // Check what follows "loop"
        size_t afterLoop = loopPos + 4;
        while (afterLoop < result.length() && isspace((unsigned char)result[afterLoop]))
            afterLoop++;
        
        if (afterLoop >= result.length()) break;
        
        bool isLoopDo = (result.substr(afterLoop, 2) == "do");
        bool isLoopBrace = (result[afterLoop] == '{');
        
        if (isLoopDo || isLoopBrace) {
            // Insert "catch(:__mkxpz_loop_break__) do " before "loop"
            // We use do...end instead of {...} to avoid precedence confusion
            std::string catchPrefix = "catch(:__mkxpz_loop_break__) do ";
            result.insert(loopPos, catchPrefix);
            
            // Re-calculate positions after insertion
            loopPos += catchPrefix.length();
            
            // Skip to after "do" or "{" to start searching for the end
            size_t blockStart = loopPos + 4; // "loop"
            while (blockStart < result.length() && isspace((unsigned char)result[blockStart]))
                blockStart++;
            
            if (isLoopDo) {
                blockStart += 2; // skip "do"
            } else {
                blockStart += 1; // skip "{"
            }
            
            // Find matching end/}
            int depth = 1;
            size_t endPos = blockStart;
            
            while (endPos < result.length() && depth > 0) {
                // Skip strings
                if (result[endPos] == '"' || result[endPos] == '\'') {
                    char quote = result[endPos++];
                    while (endPos < result.length() && result[endPos] != quote) {
                        if (result[endPos] == '\\') endPos++; // Skip escape
                        endPos++;
                    }
                    if (endPos < result.length()) endPos++;
                    continue;
                }
                
                // Skip comments
                if (result[endPos] == '#') {
                    while (endPos < result.length() && result[endPos] != '\n')
                        endPos++;
                    continue;
                }
                
                // Skip regex literals /pattern/
                // This is hard to parse perfectly without a full parser, but we can try basic heuristics
                // For now, let's skip it to avoid complexity, assuming code is well-formatted
                
                // Check for nested blocks
                if (isLoopDo) {
                    // Check for end
                    if (result.substr(endPos, 3) == "end" && 
                        (endPos + 3 >= result.length() || !isalnum(result[endPos + 3])) &&
                        (endPos == 0 || !isalnum(result[endPos - 1]))) {
                        depth--;
                        if (depth == 0) {
                            // Found the matching end
                            // Insert " end" after this "end"
                            result.insert(endPos + 3, " end");
                            modifications++;
                            break;
                        }
                        endPos += 3;
                        continue;
                    }
                    
                    // Check for block starters: do, def, class, module, case, begin, if, unless, while, until, for
                    bool startsBlock = false;
                    size_t wordLen = 0;
                    
                    if (result.substr(endPos, 2) == "do" && (endPos + 2 >= result.length() || !isalnum(result[endPos + 2]))) {
                        startsBlock = true; wordLen = 2;
                    } 
                    else if (result.substr(endPos, 3) == "def" && (endPos + 3 >= result.length() || !isalnum(result[endPos + 3]))) {
                        startsBlock = true; wordLen = 3;
                    }
                    else if ((result.substr(endPos, 5) == "class" || result.substr(endPos, 5) == "begin" || result.substr(endPos, 5) == "while" || result.substr(endPos, 5) == "until") && 
                             (endPos + 5 >= result.length() || !isalnum(result[endPos + 5]))) {
                        startsBlock = true; wordLen = 5;
                    }
                    else if ((result.substr(endPos, 6) == "module" || result.substr(endPos, 6) == "unless") && 
                             (endPos + 6 >= result.length() || !isalnum(result[endPos + 6]))) {
                        startsBlock = true; wordLen = 6;
                    }
                    else if ((result.substr(endPos, 4) == "case") && 
                             (endPos + 4 >= result.length() || !isalnum(result[endPos + 4]))) {
                        startsBlock = true; wordLen = 4;
                    }
                    else if (result.substr(endPos, 2) == "if" && (endPos + 2 >= result.length() || !isalnum(result[endPos + 2]))) {
                        startsBlock = true; wordLen = 2;
                    }
                    else if (result.substr(endPos, 3) == "for" && (endPos + 3 >= result.length() || !isalnum(result[endPos + 3]))) {
                        startsBlock = true; wordLen = 3;
                    }
                    
                    if (startsBlock) {
                        // Check previous char to avoid part of word
                        if (endPos > 0 && (isalnum(result[endPos - 1]) || result[endPos - 1] == '_')) {
                            endPos += wordLen;
                            continue;
                        }
                        
                        // Modifier check for if, unless, while, until
                        // If it's a modifier (e.g. "x = 1 if y"), it doesn't start a block
                        // Heuristic: Check if preceded by newline or semicolon (ignoring whitespace)
                        bool isModifier = false;
                        if (wordLen == 2 /*if*/ || wordLen == 6 /*unless*/ || wordLen == 5 /*while/until*/) {
                            size_t checkPos = endPos - 1;
                            while (checkPos > 0 && (result[checkPos] == ' ' || result[checkPos] == '\t'))
                                checkPos--;
                            
                            // If the previous significant char is NOT \n or ;, it's likely a modifier
                            if (checkPos > 0 && result[checkPos] != '\n' && result[checkPos] != ';') {
                                // One exception: "then if" or "else if" (elsif is separate keyword)
                                // But simple heuristic: if preceded by non-structure char, it's modifier
                                isModifier = true;
                            }
                        }
                        
                        if (!isModifier) {
                            depth++;
                        }
                        endPos += wordLen;
                        continue;
                    }
                } else {
                    // Brace matching for loop { }
                    if (result[endPos] == '{') depth++;
                    else if (result[endPos] == '}') {
                        depth--;
                        if (depth == 0) {
                            result.insert(endPos + 1, " end"); // Close the catch do block
                            modifications++;
                            break;
                        }
                    }
                }
                
                endPos++;
            }
            
            pos = endPos + 4; // Continue after the inserted " end"
        } else {
            pos = afterLoop;
        }
    }
    
    if (modifications > 0) {
        Debug() << "[MKXP-Z] SYNTAX FIX: Wrapped " << modifications << " loop blocks with catch(:__mkxpz_loop_break__) in script '" << scriptName << "'";
    }
    
    return result;
}

// Ruby 1.8 -> 3.x compatibility: Fix "retry if condition" syntax
// In Ruby 1.8, retry could be used inside loops (while, until, loop do).
// In Ruby 3.x, retry is ONLY valid inside begin...rescue...end blocks.
// This function converts "retry if condition" to "redo if condition".
static bool needsRetrySyntaxFix(const std::string &script) {
    // Quick check: look for "retry if" or "retry unless" patterns
    size_t pos = script.find("retry");
    while (pos != std::string::npos) {
        // Check if it's followed by whitespace and then "if" or "unless"
        size_t afterRetry = pos + 5;
        while (afterRetry < script.length() && (script[afterRetry] == ' ' || script[afterRetry] == '\t'))
            afterRetry++;
        
        if (afterRetry < script.length()) {
            if (script.substr(afterRetry, 2) == "if" || script.substr(afterRetry, 6) == "unless") {
                return true;
            }
        }
        
        pos = script.find("retry", pos + 1);
    }
    return false;
}

static std::string fixRetrySyntax(const std::string &script, const char* scriptName) {
    std::string result = script;
    
    // Find and replace "retry if" -> "redo if" and "retry unless" -> "redo unless"
    size_t pos = 0;
    while ((pos = result.find("retry", pos)) != std::string::npos) {
        // Check if it's followed by whitespace and then "if" or "unless"
        size_t afterRetry = pos + 5;
        while (afterRetry < result.length() && (result[afterRetry] == ' ' || result[afterRetry] == '\t'))
            afterRetry++;
        
        if (afterRetry < result.length()) {
            bool isIf = (result.substr(afterRetry, 2) == "if" && 
                        (afterRetry + 2 >= result.length() || !isalnum(result[afterRetry + 2])));
            bool isUnless = (result.substr(afterRetry, 6) == "unless" && 
                            (afterRetry + 6 >= result.length() || !isalnum(result[afterRetry + 6])));
            
            if (isIf || isUnless) {
                // Replace "retry" with "redo"
                result.replace(pos, 5, "redo");
                Debug() << "[MKXP-Z] SYNTAX FIX: Converting 'retry " << (isIf ? "if" : "unless") 
                        << "' to 'redo " << (isIf ? "if" : "unless") << "' in script '" << scriptName << "'";
            }
        }
        
        pos++;
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

static std::string fixSpecificScriptErrors(const std::string &script, const char* scriptName) {
    std::string result = script;
    std::string sName = scriptName;

    // Pokemon HGSS / Cable Club Fix: "else without rescue is useless" syntax error
    // In Ruby 1.8, 'else' was sometimes used inside 'begin' blocks without 'rescue'
    // Ruby 3.x/Prism rejects this. We safely remove the useless 'else' keyword.
    if (sName.find("Cable Club") != std::string::npos) {
        try {
            std::regex pattern(R"(begin(\s+)yield record(\s+)else)");
            if (std::regex_search(result, pattern)) {
                result = std::regex_replace(result, pattern, "begin$1yield record$2");
                Debug() << "[MKXP-Z] SYNTAX FIX: Removed useless 'else' in begin block in script '" << scriptName << "'";
            }
        } catch (const std::regex_error& e) {
            Debug() << "[MKXP-Z] Cable Club regex error: " << e.what();
        }
    }

    // Pokemon Insurgence Fix: Connect/Register/Login/Deuks double 'end' syntax error
    // Error: SyntaxError: 189:Connect/Register/Login/Deuks:959: syntax error found
    // 957 |     pbSave
    // 958 |   end
    // 959 | end
    if (sName.find("Connect/Register/Login/Deuks") != std::string::npos) {
        try {
            // Match: pbSave followed by whitespace/newlines, then 'end', whitespace, 'end'
            // We want to replace it with just one 'end' (and pbSave)
            std::regex pattern(R"(pbSave\s+end\s+end)");
            if (std::regex_search(result, pattern)) {
                // Formatting: pbSave + newline + indent + end
                result = std::regex_replace(result, pattern, "pbSave\r\n    end");
                Debug() << "[MKXP-Z] SYNTAX FIX: Fixed double 'end' syntax error in script '" << scriptName << "'";
            }
        } catch (const std::regex_error& e) {
            Debug() << "[MKXP-Z] Specific syntax fix regex error: " << e.what();
        }
    }

    // Pokemon Awakening Fix: PokeBattle_BES-T_BattleInfo uses "next" inside a def block
    // The Prism parser rejects this as "Invalid next" since next is only valid in loops/blocks.
    // Replace "next" followed by "if minus <= 0" with "return" to fix the syntax error.
    if (sName.find("PokeBattle_BES-T_BattleInfo") != std::string::npos) {
        try {
            std::regex pattern(R"(\bnext\b(?=\s+if\s+minus\s*<=\s*0))");
            if (std::regex_search(result, pattern)) {
                result = std::regex_replace(result, pattern, "return");
                Debug() << "[MKXP-Z] SYNTAX FIX: Replaced 'next' with 'return' in script '" << scriptName << "'";
            }
        } catch (const std::regex_error& e) {
            Debug() << "[MKXP-Z] PokeBattle_BES-T_BattleInfo regex error: " << e.what();
        }
    }

    return result;
}


#define SCRIPT_SECTION_FMT (rgssVer >= 3 ? "{%04ld}" : "Section%03ld")

static void runRMXPScripts(BacktraceData &btData) {
    MKXP_DEBUG_LOG("runRMXPScripts starting...");
    const Config &conf = shState->rtData().config;
    const std::string &scriptPack = conf.game.scripts;
    MKXP_DEBUG_LOG("Script pack path: %s\n", scriptPack.c_str());
    
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
    MKXP_DEBUG_LOG("Loaded %ld scripts from archive\n", scriptCount);
    
    std::string decodeBuffer;
    // Increased from 0x1000 (4KB) to 0x300000 (3MB) to prevent thread crashes
    // when decompressing large script files. Older RPG Maker games often have
    // Scripts.rxdata files that decompress to very large scripts.
    decodeBuffer.resize(0x300000);
    size_t initialBufferSize = decodeBuffer.size();
    MKXP_DEBUG_LOG("Initial decode buffer size: %zu bytes (%.1f MB)\n", 
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
        
        // Fix retry syntax (Ruby 1.8 -> 3.x compatibility)
        // In Ruby 1.8, "retry if condition" was valid in loops. In Ruby 3.x, it's a syntax error.
        if (needsRetrySyntaxFix(processedScript)) {
            processedScript = fixRetrySyntax(processedScript, RSTRING_PTR(scriptName));
        }
        
        // Ruby 3.x compatibility: wrap scripts that use @@class_variables in toplevel singleton class
        // This fixes "class variable access from toplevel" RuntimeError
        if (needsClassVariableCompat(processedScript)) {
            processedScript = wrapScriptForClassVariableCompat(processedScript, RSTRING_PTR(scriptName));
        }

        // =========================================================================
        // PluginManager Superclass Mismatch Fix
        // =========================================================================
        // Intercept 'eval' calls inside PluginManager to catch Game_Temp superclass mismatch.
        // We replace 'eval(' with 'MKXPZSuperclassFix.wrap_eval(binding, '
        // passing the current binding to maintain scope.
        std::string sName = RSTRING_PTR(scriptName);
        if (sName.find("PluginManager") != std::string::npos) {
             size_t start_pos = 0;
             // Regex replacement would be safer but basic string replace works for 'eval('
             while((start_pos = processedScript.find("eval(", start_pos)) != std::string::npos) {
                 // Verify it's not part of another word like 'module_eval('
                 if (start_pos == 0 || (!isalnum(processedScript[start_pos-1]) && processedScript[start_pos-1] != '_')) {
                     processedScript.replace(start_pos, 5, "MKXPZSuperclassFix.wrap_eval(binding, ");
                     start_pos += 37; // Length of replacement string
                     Debug() << "[MKXP-Z] Patched PluginManager eval call to use SuperclassFix";
                 } else {
                     start_pos += 5;
                 }
             }
        }
        
        // =========================================================================
        // Break Syntax Fix for Ruby 4.0 Prism Parser (script-specific)
        // =========================================================================
        // Fix "Invalid break" syntax errors in known problematic scripts.
        // This replaces standalone "break" with "throw(:__mkxpz_loop_break__)" and
        // wraps "loop do" blocks with "catch(:__mkxpz_loop_break__)".
        if (needsBreakSyntaxFix(processedScript, sName)) {
            processedScript = fixBreakSyntax(processedScript, RSTRING_PTR(scriptName));
            processedScript = wrapLoopsWithCatch(processedScript, RSTRING_PTR(scriptName));
        }
        


        // Apply specific internal syntax fixes (e.g. for Pokemon Insurgence)
        processedScript = fixSpecificScriptErrors(processedScript, RSTRING_PTR(scriptName));
        
        rb_ary_store(script, 3, rb_utf8_str_new_cstr(processedScript.c_str()));
    }
    
    fprintf(stderr, "[MKXP-Z] ========== SCRIPT LOADING COMPLETE ==========\n");
    fprintf(stderr, "[MKXP-Z] Total scripts loaded: %ld\n", scriptCount);
    
    /* Execute preloaded scripts */
    for (std::vector<std::string>::const_iterator i = conf.preloadScripts.begin();
         i != conf.preloadScripts.end(); ++i)
    {
        if (shState->rtData().rqTerm)
            break;
        fprintf(stderr, "[MKXP-Z] Running preload script: %s\n", i->c_str());
        runCustomScript(*i);
    }
    
    VALUE exc = rb_gv_get("$!");
    if (exc != Qnil) {
        fprintf(stderr, "[MKXP-Z] ERROR: Exception before main loop! Aborting.\n");
        return;
    }
    
    fprintf(stderr, "[MKXP-Z] ========== STARTING MAIN SCRIPT EXECUTION ==========\n");
    while (true) {
        for (long i = 0; i < scriptCount; ++i) {
            if (shState->rtData().rqTerm) {
                fprintf(stderr, "[MKXP-Z] Termination requested at script %ld\n", i);
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
            
            // VERBOSE LOGGING: Log every script
            fprintf(stderr, "[MKXP-Z] [%03ld/%ld] Executing: %s\n", i, scriptCount, scriptName);
            
            // ================================================================
            // WIN32API POSTLOAD PATCH - Run before Main script
            // This patches GetAsyncKeyState/GetKeyState to use Input.raw_key_states
            // which allows virtual gamepad input to be detected by games using Win32 API
            // ================================================================
            if (strcmp(scriptName, "Main") == 0) {
                fprintf(stderr, "[MKXP-Z] Applying Win32API GetAsyncKeyState postload patch...\n");
                
                const char* win32apiPatch = R"RUBY(
# Win32API GetAsyncKeyState/GetKeyState Postload Patch
# Patches the game's Win32API class to use Input.raw_key_states
# for proper virtual gamepad support on iOS

if Object.const_defined?(:Win32API)
  puts "[MKXP-Z] Patching Win32API for GetAsyncKeyState/GetKeyState..."
  
  # Windows VK to SDL Scancode mapping (comprehensive)
  $__mkxpz_vk_to_sdl = {
    0x0D => 40,   # VK_RETURN -> SDL_SCANCODE_RETURN (0x28)
    0x1B => 41,   # VK_ESCAPE -> SDL_SCANCODE_ESCAPE (0x29)
    0x20 => 44,   # VK_SPACE -> SDL_SCANCODE_SPACE (0x2C)
    0x08 => 42,   # VK_BACK -> SDL_SCANCODE_BACKSPACE (0x2A)
    0x09 => 43,   # VK_TAB -> SDL_SCANCODE_TAB (0x2B)
    0x25 => 80,   # VK_LEFT -> SDL_SCANCODE_LEFT (0x50)
    0x26 => 82,   # VK_UP -> SDL_SCANCODE_UP (0x52)
    0x27 => 79,   # VK_RIGHT -> SDL_SCANCODE_RIGHT (0x4F)
    0x28 => 81,   # VK_DOWN -> SDL_SCANCODE_DOWN (0x51)
    0x10 => -10,  # VK_SHIFT -> combined LSHIFT/RSHIFT
    0xA0 => 225,  # VK_LSHIFT -> SDL_SCANCODE_LSHIFT (0xE1)
    0xA1 => 229,  # VK_RSHIFT -> SDL_SCANCODE_RSHIFT (0xE5)
    0x11 => -11,  # VK_CONTROL -> combined LCTRL/RCTRL
    0xA2 => 224,  # VK_LCONTROL -> SDL_SCANCODE_LCTRL (0xE0)
    0xA3 => 228,  # VK_RCONTROL -> SDL_SCANCODE_RCTRL (0xE4)
    0x12 => -12,  # VK_MENU -> combined LALT/RALT
    0xA4 => 226,  # VK_LMENU -> SDL_SCANCODE_LALT (0xE2)
    0xA5 => 230,  # VK_RMENU -> SDL_SCANCODE_RALT (0xE6)
  }
  
  # Add A-Z (VK 0x41-0x5A -> SDL_SCANCODE_A-Z = 4-29)
  (0x41..0x5A).each { |vk| $__mkxpz_vk_to_sdl[vk] = vk - 0x41 + 4 }
  # Add 0-9 (VK 0x30-0x39 -> SDL_SCANCODE_0=39, SDL_SCANCODE_1-9=30-38)
  $__mkxpz_vk_to_sdl[0x30] = 39  # 0 -> SDL_SCANCODE_0
  (0x31..0x39).each { |vk| $__mkxpz_vk_to_sdl[vk] = vk - 0x31 + 30 }
  # Add F1-F12 (VK 0x70-0x7B -> SDL_SCANCODE_F1-F12 = 58-69)
  (0x70..0x7B).each { |vk| $__mkxpz_vk_to_sdl[vk] = vk - 0x70 + 58 }
  
  # Define global Proc for checking key state (avoids scope issues with define_method)
  $__mkxpz_check_keystate_proc ||= lambda do |vk_code|
    begin
      states = Input.raw_key_states rescue nil
      return 0 unless states
      
      sdl_scan = $__mkxpz_vk_to_sdl[vk_code]
      
      pressed = false
      if sdl_scan
        if sdl_scan == -10  # Combined Shift
          pressed = (states[225] rescue false) || (states[229] rescue false)
        elsif sdl_scan == -11  # Combined Control
          pressed = (states[224] rescue false) || (states[228] rescue false)
        elsif sdl_scan == -12  # Combined Alt
          pressed = (states[226] rescue false) || (states[230] rescue false)
        else
          pressed = states[sdl_scan] rescue false
        end
      end
      
      # ALIASES for iOS Virtual Gamepad Interop:
      # Games often check Z/X keys (0x5A/0x58) but the virtual gamepad 
      # might behave as Return/Escape or vice versa.
      unless pressed
        # If Game checks VK_Z (0x5A) -> Also check Enter (40) and Space (44)
        if vk_code == 0x5A
          pressed = (states[40] rescue false) || (states[44] rescue false)
        # If Game checks VK_X (0x58) -> Also check Escape (41)
        elsif vk_code == 0x58
          pressed = (states[41] rescue false)
        # If Game checks VK_RETURN (0x0D) -> Also check Z (29) and Space (44)
        elsif vk_code == 0x0D
            pressed = (states[29] rescue false) || (states[44] rescue false)
        # If Game checks VK_ESCAPE (0x1B) -> Also check X (27)
        elsif vk_code == 0x1B
            pressed = (states[27] rescue false)
        # If Game checks VK_C (0x43) -> Check Enter(40), Z(29), Space(44)
        elsif vk_code == 0x43
          pressed = (states[40] rescue false) || (states[29] rescue false) || (states[44] rescue false)
        end
      end
      
      return pressed ? 1 : 0
    rescue => e
      puts "[MKXP-Z] Key state check error: #{e.message}"
      return 0
    end
  end
  
  begin
    win32api_class = Object.const_get(:Win32API)
    
    # Store original call method
    if win32api_class.instance_methods.include?(:call)
      original_call = win32api_class.instance_method(:call)
      
      # Patch the call method - using class_eval to access define_method safely if private
      win32api_class.class_eval do
        define_method(:call) do |*args|
          func_lower = (@func || @function || '').to_s.downcase
          dll_lower = (@dll || @dllname || '').to_s.downcase
          
          if dll_lower.include?('user32')
            if func_lower == 'getasynckeystate'
              vk_code = args[0].to_i rescue 0
              result = $__mkxpz_check_keystate_proc.call(vk_code)
              return result == 1 ? 0x8000 : 0
            elsif func_lower == 'getkeystate'
              vk_code = args[0].to_i rescue 0
              result = $__mkxpz_check_keystate_proc.call(vk_code)
              # Fix: GetKeyState also returns high-order bit for pressed status
              return result == 1 ? 0x8000 : 0
            end
          end
          
          # Call original for other functions
          original_call.bind(self).call(*args)
        end
      end
      
      puts "[MKXP-Z] [OK] Win32API.call patched for GetAsyncKeyState/GetKeyState (global Proc + Aliases)"
    else
      puts "[MKXP-Z] Win32API has no 'call' method to patch"
    end
  rescue => e
    puts "[MKXP-Z] Warning: Could not patch Win32API: #{e.message}"
    puts e.backtrace.first(3).join("\n")
  end
else
  puts "[MKXP-Z] Win32API class not defined, skipping patch"
end

# ---------- NEW: MiniFFI Patch ----------
if Object.const_defined?(:MiniFFI)
  puts "[MKXP-Z] Patching MiniFFI..."
  miniffi = Object.const_get(:MiniFFI)
  
  # Patch pbFindRgssWindow
  miniffi.define_singleton_method(:pbFindRgssWindow) do
    puts "[MKXP-Z] MiniFFI.pbFindRgssWindow (postload-patched) - returning mock handle"
    return 0x12345678
  end
  
  # Patch restoreScreen
  miniffi.define_singleton_method(:restoreScreen) do
    return true
  end
  
  # Patch getWindowRect
  miniffi.define_singleton_method(:getWindowRect) do |*args|
    w = $SCREEN_WIDTH || 640
    h = $SCREEN_HEIGHT || 480
    return [0, 0, w, h]
  end
  
  # Patch setWindowPos / SetWindowPos
  miniffi.define_singleton_method(:setWindowPos) { |*args| return true }
  miniffi.define_singleton_method(:SetWindowPos) { |*args| return true }
  
  puts "[MKXP-Z] [OK] MiniFFI methods patched"
else
  puts "[MKXP-Z] MiniFFI class not defined yet, skipping patch"
end
)RUBY";
                
                int patchState;
                rb_eval_string_protect(win32apiPatch, &patchState);
                
                if (patchState) {
                    VALUE exc = rb_errinfo();
                    if (exc != Qnil) {
                        VALUE msg = rb_funcall(exc, rb_intern("message"), 0);
                        fprintf(stderr, "[MKXP-Z] Win32API patch error: %s\n", StringValueCStr(msg));
                        rb_set_errinfo(Qnil);
                    }
                } else {
                    fprintf(stderr, "[MKXP-Z] Win32API postload patch applied successfully\n");
                }
            }
            
            
            evalString(string, fname, &state);
            
            if (state == 0) {
                // Script executed successfully
                if (i < 10 || i == scriptCount - 1 || i % 20 == 0) {
                    fprintf(stderr, "[MKXP-Z] [%03ld/%ld] ✓ Success: %s\n", i, scriptCount, scriptName);
                }
                
                // ================================================================
                // WIN32API POSTLOAD PATCH
                // Run after Win32API script or before Main script
                // ================================================================
                static bool win32api_patched = false;
                bool should_patch = false;
                
                // Check if we should patch
                if (!win32api_patched) {
                    if (strcmp(scriptName, "Win32API") == 0) {
                        fprintf(stderr, "[MKXP-Z] Win32API script execution detected - Applying patch...\n");
                        should_patch = true;
                    } else if (strcmp(scriptName, "Main") == 0) {
                        fprintf(stderr, "[MKXP-Z] Main script detected (Win32API not patched yet) - Applying patch...\n");
                        should_patch = true;
                    }
                }
                
                if (should_patch) {
                    const char* win32apiPatch = R"RUBY(
# Win32API GetAsyncKeyState/GetKeyState Postload Patch
if Object.const_defined?(:Win32API)
  puts "[MKXP-Z] Patching Win32API for GetAsyncKeyState/GetKeyState..."
  
  # Windows VK to SDL Scancode mapping (comprehensive)
  $__mkxpz_vk_to_sdl = {
    0x0D => 40,   # VK_RETURN -> SDL_SCANCODE_RETURN
    0x1B => 41,   # VK_ESCAPE -> SDL_SCANCODE_ESCAPE
    0x20 => 44,   # VK_SPACE -> SDL_SCANCODE_SPACE
    0x08 => 42,   # VK_BACK -> SDL_SCANCODE_BACKSPACE
    0x09 => 43,   # VK_TAB -> SDL_SCANCODE_TAB
    0x25 => 80,   # VK_LEFT -> SDL_SCANCODE_LEFT
    0x26 => 82,   # VK_UP -> SDL_SCANCODE_UP
    0x27 => 79,   # VK_RIGHT -> SDL_SCANCODE_RIGHT
    0x28 => 81,   # VK_DOWN -> SDL_SCANCODE_DOWN
    0x10 => -10,  # VK_SHIFT -> combined LSHIFT/RSHIFT
    0xA0 => 225,  # VK_LSHIFT -> SDL_SCANCODE_LSHIFT
    0xA1 => 229,  # VK_RSHIFT -> SDL_SCANCODE_RSHIFT
    0x11 => -11,  # VK_CONTROL -> combined LCTRL/RCTRL
    0xA2 => 224,  # VK_LCONTROL -> SDL_SCANCODE_LCTRL
    0xA3 => 228,  # VK_RCONTROL -> SDL_SCANCODE_RCTRL
    0x12 => -12,  # VK_MENU -> combined LALT/RALT
    0xA4 => 226,  # VK_LMENU -> SDL_SCANCODE_LALT
    0xA5 => 230,  # VK_RMENU -> SDL_SCANCODE_RALT
  }
  
  # Add A-Z (VK 0x41-0x5A -> SDL_SCANCODE_A-Z = 4-29)
  (0x41..0x5A).each { |vk| $__mkxpz_vk_to_sdl[vk] = vk - 0x41 + 4 }
  # Add 0-9 (VK 0x30-0x39 -> SDL_SCANCODE_0=39, SDL_SCANCODE_1-9=30-38)
  $__mkxpz_vk_to_sdl[0x30] = 39  # 0 -> SDL_SCANCODE_0
  (0x31..0x39).each { |vk| $__mkxpz_vk_to_sdl[vk] = vk - 0x31 + 30 }
  # Add F1-F12 (VK 0x70-0x7B -> SDL_SCANCODE_F1-F12 = 58-69)
  (0x70..0x7B).each { |vk| $__mkxpz_vk_to_sdl[vk] = vk - 0x70 + 58 }
  
  # Define global Proc for checking key state (avoids scope issues with define_method)
  $__mkxpz_check_keystate_proc ||= lambda do |vk_code|
    begin
      states = Input.raw_key_states rescue nil
      return 0 unless states
      
      sdl_scan = $__mkxpz_vk_to_sdl[vk_code]
      
      pressed = false
      if sdl_scan
        if sdl_scan == -10  # Combined Shift
          pressed = (states[225] rescue false) || (states[229] rescue false)
        elsif sdl_scan == -11  # Combined Control
          pressed = (states[224] rescue false) || (states[228] rescue false)
        elsif sdl_scan == -12  # Combined Alt
          pressed = (states[226] rescue false) || (states[230] rescue false)
        else
          pressed = states[sdl_scan] rescue false
        end
      end
      
      # ALIASES for iOS Virtual Gamepad Interop:
      unless pressed
        # If Game checks VK_Z (0x5A) -> Also check Enter (40) and Space (44)
        if vk_code == 0x5A
          pressed = (states[40] rescue false) || (states[44] rescue false)
        # If Game checks VK_X (0x58) -> Also check Escape (41)
        elsif vk_code == 0x58
          pressed = (states[41] rescue false)
        # If Game checks VK_RETURN (0x0D) -> Also check Z (29) and Space (44)
        elsif vk_code == 0x0D
            pressed = (states[29] rescue false) || (states[44] rescue false)
        # If Game checks VK_ESCAPE (0x1B) -> Also check X (27)
        elsif vk_code == 0x1B
            pressed = (states[27] rescue false)
        # If Game checks VK_C (0x43) -> Check Enter(40), Z(29), Space(44)
        elsif vk_code == 0x43
          pressed = (states[40] rescue false) || (states[29] rescue false) || (states[44] rescue false)
        end
      end
      
      return pressed ? 1 : 0
    rescue
      return 0
    end
  end
  
  begin
    win32api_class = Object.const_get(:Win32API)
    
    if win32api_class.instance_methods.include?(:call)
      original_call = win32api_class.instance_method(:call)
      
      # Patch the call method - using class_eval to access define_method safely
      win32api_class.class_eval do
        define_method(:call) do |*args|
          func_lower = (@func || @function || '').to_s.downcase
          dll_lower = (@dll || @dllname || '').to_s.downcase
          
          if dll_lower.include?('user32')
            if func_lower == 'getasynckeystate'
              vk_code = args[0].to_i rescue 0
              result = $__mkxpz_check_keystate_proc.call(vk_code)
              return result == 1 ? 0x8000 : 0
            elsif func_lower == 'getkeystate'
              vk_code = args[0].to_i rescue 0
              result = $__mkxpz_check_keystate_proc.call(vk_code)
              # Fix: GetKeyState also returns high-order bit for pressed status
              return result == 1 ? 0x8000 : 0
            end
          end
          original_call.bind(self).call(*args)
        end
      end
      puts "[MKXP-Z] [OK] Win32API.call patched for GetAsyncKeyState/GetKeyState (global Proc + Aliases)"
    end
  rescue => e
    puts "[MKXP-Z] Warning: Could not patch Win32API: #{e.message}"
  end
end

# ---------- NEW: MiniFFI Patch ----------
if Object.const_defined?(:MiniFFI)
  puts "[MKXP-Z] Patching MiniFFI..."
  miniffi = Object.const_get(:MiniFFI)
  
  # Patch pbFindRgssWindow
  miniffi.define_singleton_method(:pbFindRgssWindow) do
    puts "[MKXP-Z] MiniFFI.pbFindRgssWindow (postload-patched) - returning mock handle"
    return 0x12345678
  end
  
  # Patch restoreScreen
  miniffi.define_singleton_method(:restoreScreen) do
    return true
  end
  
  # Patch getWindowRect
  miniffi.define_singleton_method(:getWindowRect) do |*args|
    w = $SCREEN_WIDTH || 640
    h = $SCREEN_HEIGHT || 480
    return [0, 0, w, h]
  end
  
  # Patch setWindowPos / SetWindowPos
  miniffi.define_singleton_method(:setWindowPos) { |*args| return true }
  miniffi.define_singleton_method(:SetWindowPos) { |*args| return true }
  
  puts "[MKXP-Z] [OK] MiniFFI methods patched"
else
  puts "[MKXP-Z] MiniFFI class not defined yet, skipping patch"
end
)RUBY";
                    int patchState;
                    rb_eval_string_protect(win32apiPatch, &patchState);
                    if (!patchState) {
                        win32api_patched = true;
                        fprintf(stderr, "[MKXP-Z] Win32API postload patch applied successfully\n");
                    }
                }
                
                // Special logging for Main script
                if (strcmp(scriptName, "Main") == 0) {
                    fprintf(stderr, "[MKXP-Z] ========== MAIN SCRIPT FINISHED ==========\n");
                    fprintf(stderr, "[MKXP-Z] Game loop has ended normally.\n");
                }
            }
            
                // Handle superclass mismatch error specially
            if (state) {
                VALUE exc = rb_errinfo();
                if (exc != Qnil && rb_obj_is_kind_of(exc, rb_eTypeError)) {
                    VALUE msg = rb_funcall(exc, rb_intern("message"), 0);
                    const char* msgStr = StringValueCStr(msg);
                    
                    if (strstr(msgStr, "superclass mismatch") != nullptr) {
                        // Clear the error
                        rb_set_errinfo(Qnil);
                        
                        // Strategy: Remove the existing class definition and let the script define it fresh
                        // This solves mismatch and avoids infinite recursion from alias methods mixing old/new methods
                        
                        const char* classStart = strstr(msgStr, "for class ");
                        if (classStart) {
                            classStart += 10; // Skip "for class "
                            std::string className;
                            while (*classStart && (isalnum(*classStart) || *classStart == '_')) {
                                className += *classStart++;
                            }
                            
                            if (!className.empty()) {
                                fprintf(stderr, "[MKXP-Z] Superclass mismatch for %s - Removing existing class definition to fix...\n", className.c_str());
                                
                                // Remove the class constant from Object
                                // This makes the class "undefined" so the script can define it from scratch
                                ID classId = rb_intern(className.c_str());
                                if (rb_const_defined(rb_cObject, classId)) {
                                    rb_const_remove(rb_cObject, classId);
                                    
                                    // Retry executing the ORIGINAL script
                                    rb_eval_string_protect(RSTRING_PTR(scriptDecoded), &state);
                                    
                                    if (!state) {
                                        fprintf(stderr, "[MKXP-Z] [%03ld/%ld] ✓ Fixed superclass mismatch for %s (class redefined)\n", i, scriptCount, className.c_str());
                                        continue;
                                    } else {
                                         // Failed again
                                         VALUE newExc = rb_errinfo();
                                         VALUE newMsg = rb_funcall(newExc, rb_intern("message"), 0);
                                         fprintf(stderr, "[MKXP-Z] Redefinition failing: %s. SKIPPING script: %s\n", StringValueCStr(newMsg), scriptName);
                                         rb_set_errinfo(Qnil);
                                         continue;
                                    }
                                }
                            }
                        }
                        
                        // Fallback if class extraction failed
                        fprintf(stderr, "[MKXP-Z] [%03ld/%ld] ⚠️  Superclass mismatch in '%s' - SKIPPING script\n", 
                                i, scriptCount, scriptName);
                        continue;
                    }
                }
            }
            
            // If there was still an error (non-superclass-mismatch), log and break
            if (state) {
                // Log the error details
                fprintf(stderr, "[MKXP-Z] [%03ld/%ld] ERROR in script: %s\n", i, scriptCount, scriptName);
                fprintf(stderr, "[MKXP-Z] Error state: %d\n", state);
                
                // Try to get error details
                VALUE errInfo = rb_errinfo();
                if (errInfo != Qnil) {
                    VALUE errClass = rb_class_name(rb_obj_class(errInfo));
                    VALUE errMsg = rb_funcall(errInfo, rb_intern("message"), 0);
                    const char* msgStr = StringValueCStr(errMsg);
                    const char* classStr = StringValueCStr(errClass);
                    fprintf(stderr, "[MKXP-Z] Exception: %s: %s\n", classStr, msgStr);
                    
                    // =================================================================
                    // "Invalid next" / "Invalid break" SyntaxError Auto-Fix
                    // =================================================================
                    // Modern Ruby's Prism parser rejects next/break inside def blocks.
                    // Older RPG Maker scripts sometimes use next/break where return
                    // should be used. We detect this at runtime and fix it via regex.
                    if (strstr(classStr, "SyntaxError") != nullptr &&
                        (strstr(msgStr, "Invalid next") != nullptr || strstr(msgStr, "Invalid break") != nullptr)) {
                        
                        fprintf(stderr, "[MKXP-Z] Detected Invalid next/break SyntaxError in '%s', attempting auto-fix...\n", scriptName);
                        
                        // Get the original script content
                        std::string scriptContent = RSTRING_PTR(scriptDecoded);
                        bool fixed = false;
                        
                        try {
                            if (strstr(msgStr, "Invalid next") != nullptr) {
                                std::regex nextPattern(R"(\bnext\b)");
                                scriptContent = std::regex_replace(scriptContent, nextPattern, "return");
                                fprintf(stderr, "[MKXP-Z] Replaced all 'next' with 'return' in '%s'\n", scriptName);
                                fixed = true;
                            }
                            if (strstr(msgStr, "Invalid break") != nullptr) {
                                std::regex breakPattern(R"(\bbreak\b)");
                                scriptContent = std::regex_replace(scriptContent, breakPattern, "return");
                                fprintf(stderr, "[MKXP-Z] Replaced all 'break' with 'return' in '%s'\n", scriptName);
                                fixed = true;
                            }
                        } catch (const std::regex_error& e) {
                            fprintf(stderr, "[MKXP-Z] Regex error during Invalid next/break fix: %s\n", e.what());
                        }
                        
                        if (fixed) {
                            // Clear the error and retry with the fixed script
                            rb_set_errinfo(Qnil);
                            VALUE fixedString = rb_utf8_str_new_cstr(scriptContent.c_str());
                            int retryState;
                            evalString(fixedString, fname, &retryState);
                            
                            if (retryState == 0) {
                                fprintf(stderr, "[MKXP-Z] [%03ld/%ld] Fixed Invalid next/break in '%s' (replaced with return)\n", i, scriptCount, scriptName);
                                continue;
                            } else {
                                // Still failed after fix
                                VALUE retryExc = rb_errinfo();
                                if (retryExc != Qnil) {
                                    VALUE retryMsg = rb_funcall(retryExc, rb_intern("message"), 0);
                                    fprintf(stderr, "[MKXP-Z] Auto-fix failed, new error: %s\n", StringValueCStr(retryMsg));
                                }
                                rb_set_errinfo(Qnil);
                                fprintf(stderr, "[MKXP-Z] SKIPPING script after failed auto-fix: %s\n", scriptName);
                                continue;
                            }
                        }
                    }
                    
                    // Get backtrace
                    VALUE bt = rb_funcall(errInfo, rb_intern("backtrace"), 0);
                    if (RB_TYPE_P(bt, RUBY_T_ARRAY)) {
                        long btLen = RARRAY_LEN(bt);
                        fprintf(stderr, "[MKXP-Z] Backtrace (%ld frames):\n", btLen);
                        for (long j = 0; j < btLen && j < 10; j++) {
                            VALUE line = rb_ary_entry(bt, j);
                            fprintf(stderr, "[MKXP-Z]   %ld: %s\n", j, StringValueCStr(line));
                        }
                    }
                }
                
                break;
            }
        }
        
        fprintf(stderr, "[MKXP-Z] ========== SCRIPT EXECUTION LOOP COMPLETE ==========\n");
        
        VALUE exc = rb_gv_get("$!");
        if (exc != Qnil) {
            VALUE excClass = rb_class_name(rb_obj_class(exc));
            fprintf(stderr, "[MKXP-Z] Post-loop exception: %s\n", StringValueCStr(excClass));
        }
        
        if (rb_obj_class(exc) != getRbData()->exc[Reset]) {
            fprintf(stderr, "[MKXP-Z] Breaking main loop (not a Reset exception)\n");
            break;
        }
        
        if (processReset(false)) {
            fprintf(stderr, "[MKXP-Z] processReset returned true, breaking\n");
            break;
        }
        
        fprintf(stderr, "[MKXP-Z] Reset processed, restarting script loop...\n");
    }
    
    fprintf(stderr, "[MKXP-Z] ========== EXITING runRMXPScripts ==========\n");
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
    MKXP_DEBUG_LOG("mriBindingExecute started, checking shState...");
    if (!shState) {
        fprintf(stderr, "[MKXP-Z] ERROR: SharedState is null! Cannot proceed.\n");
        return;
    }
    MKXP_DEBUG_LOG("shState is valid (%p)\n", (void*)shState);
    
    Config &conf = shState->rtData().config;
    MKXP_DEBUG_LOG("Config loaded, RGSS version: %d\n", conf.rgssVersion);
    
    // iOS Font Fix: Increase fontScale if it's at default value
    // Pokemon Essentials and other games using custom fonts may have clipped text
    // because iOS screen scaling differs from Windows. A higher fontScale helps
    // ensure the full glyph height is rendered.
    if (conf.fontScale < 0.1f || conf.fontScale == 1.0f) {
        conf.fontScale = 1.5f;
        MKXP_INFO_LOG("iOS font fix applied - fontScale set to %.1f\n", conf.fontScale);
    }
    
#if RAPI_MAJOR >= 2
    // Ruby initialization can only happen ONCE per process
    // Calling ruby_init() again will cause "encoding name was somehow registered twice" crash
    if (!rubyInitialized) {
        /* Normally only a ruby executable would do a sysinit,
         * but not doing it will lead to crashes due to closed
         * stdio streams on some platforms (eg. Windows) */
        // iOS fix: Provide valid argc/argv to avoid null pointer crash in ruby_sysinit
        MKXP_DEBUG_LOG("About to call ruby_sysinit...");
        static char progname[] = "mkxp-z";
        static char *ios_argv[] = { progname, nullptr };
        int argc = 1;
        char **argv = ios_argv;
        ruby_sysinit(&argc, &argv);
        MKXP_DEBUG_LOG("ruby_sysinit completed");
        
        // iOS fix: Use explicit ruby_init_stack with LOCAL variable from function top scope
        // CRITICAL: The stack variable MUST remain in scope for the ENTIRE Ruby execution!
        // Ruby uses this to determine the stack base for GC scanning.
        // If the variable goes out of scope, the GC will access invalid memory causing EXC_BAD_ACCESS.
        // We use the volatile stack_anchor_start declared at the top of this function.
        MKXP_DEBUG_LOG("About to call ruby_init_stack with stack anchor at %p...\n", 
                (void*)&stack_anchor_start);
        ruby_init_stack((void*)&stack_anchor_start);
        MKXP_DEBUG_LOG("ruby_init_stack completed");
        
        MKXP_DEBUG_LOG("About to call ruby_init...");
        ruby_init();
        MKXP_DEBUG_LOG("ruby_init completed");
        
        // Initialize statically linked Ruby extensions (including zlib)
        MKXP_DEBUG_LOG("About to call Init_ext for static extensions...");
        Init_ext();
        MKXP_DEBUG_LOG("Init_ext completed - static extensions loaded");
        
        rubyInitialized = true;
    } else {
        MKXP_DEBUG_LOG("Ruby already initialized, skipping init calls");
        // Even when reusing Ruby, we need to ensure the stack is properly anchored
        // This is important for proper GC behavior across game launches
        ruby_init_stack((void*)&stack_anchor_start);
    }
    
    MKXP_DEBUG_LOG("About to call ruby_options...");
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
    MKXP_DEBUG_LOG("ruby_options completed, node=%p\n", node);
    
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
    
    // CRITICAL: Do NOT call ruby_cleanup() here!
    // In Ruby 3.x, ruby_cleanup() destroys the Ruby VM completely.
    // Once destroyed, ruby_init() cannot be called again (causes "encoding name registered twice" error).
    // Instead, we keep the Ruby VM alive and reuse it for subsequent game launches.
    // This is safe because we reset all game-specific state when a new game starts.
    // Memory usage is acceptable since Ruby VM is ~10-20MB.
    // 
    // Previously: ruby_cleanup(0);  // This was causing EXC_BAD_ACCESS on second game launch
    //
    // If you really need cleanup (rare), uncomment and set rubyInitialized = false:
    // ruby_cleanup(0);
    // rubyInitialized = false;
    
    MKXP_DEBUG_LOG("Game execution complete, keeping Ruby VM alive for potential next game");
    
    // Clear error info for next game session
    rb_set_errinfo(Qnil);
    
    // Mark end of Ruby execution scope - this helps ensure stack_anchor_start stays in scope
    stack_anchor_end = 1;
    (void)stack_anchor_end;  // Suppress unused variable warning
    
    shState->rtData().rqTermAck.set();
}

static void mriBindingTerminate() { throw Exception(Exception::SystemExit, " "); }

static void mriBindingReset() { throw Exception(Exception::Reset, " "); }
